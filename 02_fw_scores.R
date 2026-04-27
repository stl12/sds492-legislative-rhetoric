# 02_fw_scores.R
# Builds Fightin' Words dictionary from GS + ConSpeak corpus sample,
# scores primary loser speeches, aggregates to member-period level.
# Output: fw.rds, fw_dictionary.rds
# Please note: This script is computationally intensive (~a few hours).
# Pre-computed outputs (fw.rds, fw_dictionary.rds) are provided in data/processed/.
# To skip rerunning, proceed directly to 06_covariates.R.

library(tidyverse)
library(tidytext)
library(SnowballC)

# Text preprocessing functions

clean_text <- function(text) {
  if (is.na(text) || text == "") return("")
  text %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ") %>%
    tolower() %>%
    str_replace_all("-|'", " ") %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}

stem_words <- function(text) {
  if (is.na(text) || text == "") return("")
  words   <- str_split(text, "\\s+")[[1]]
  words   <- words[str_length(words) > 1]
  stemmed <- wordStem(words, language = "english")
  paste(stemmed, collapse = " ")
}

score_fw <- function(text_stemmed, dict) {
  if (is.na(text_stemmed) || text_stemmed == "") return(0)
  bigrams       <- tibble(text = text_stemmed) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    pull(bigram)
  if (length(bigrams) == 0) return(0)
  bigram_counts <- table(bigrams)
  matched <- dict %>%
    filter(bigram %in% names(bigram_counts)) %>%
    mutate(count = as.integer(bigram_counts[bigram]))
  if (nrow(matched) == 0) return(0)
  sum(matched$fw_score * matched$count) / sum(matched$count)
}

# PART 1: Sample GS corpus (83rd-103rd Congress)
# 2000 speeches per Congress per party

gs_dir <- "data/raw/hein-bound"

gs_congress_nums <- list.files(gs_dir, pattern = "speeches_\\d+\\.txt") %>%
  str_extract("\\d+") %>%
  as.integer() %>%
  sort() %>%
  .[. >= 83 & . <= 103]

sample_gs_congress <- function(congress_num, n_per_party = 2000) {
  congress_str <- sprintf("%03d", congress_num)
  
  speeches <- read_delim(
    file.path(gs_dir, paste0("speeches_", congress_str, ".txt")),
    delim = "|", col_names = TRUE, show_col_types = FALSE,
    locale = locale(encoding = "latin1")
  ) %>% rename(text = speech)
  
  speakermap <- read_delim(
    file.path(gs_dir, paste0(congress_str, "_SpeakerMap.txt")),
    delim = "|", col_names = TRUE, show_col_types = FALSE
  ) %>%
    filter(chamber == "H", party %in% c("D", "R")) %>%
    mutate(party = if_else(party == "D", "Democrat", "Republican")) %>%
    select(speech_id, party)
  
  combined <- speeches %>%
    inner_join(speakermap, by = "speech_id") %>%
    mutate(text = iconv(text, from = "latin1", to = "UTF-8", sub = " ")) %>%
    filter(!is.na(text), nchar(text) > 300) %>%
    mutate(congress = congress_num) %>%
    select(congress, party, text)
  
  set.seed(1234 + congress_num)
  bind_rows(
    combined %>% filter(party == "Democrat")   %>% slice_sample(n = min(n_per_party, nrow(.))),
    combined %>% filter(party == "Republican") %>% slice_sample(n = min(n_per_party, nrow(.)))
  )
}

gs_sample <- map_df(gs_congress_nums, function(cong) {
  tryCatch(sample_gs_congress(cong), error = function(e) NULL)
})

write_rds(gs_sample, "data/processed/gs_fw_sample.rds")

# PART 2: Sample ConSpeak corpus (104th-118th Congress)
# 2000 speeches per Congress per party

cs_files <- list.files("data/raw/dataverse_files",
                       pattern = "\\.json$", full.names = TRUE)

cs_index <- map_df(cs_files, function(f) {
  tryCatch({
    data <- jsonlite::fromJSON(f)
    if (is.null(data$speeches)) return(NULL)
    data$speeches %>%
      as_tibble() %>%
      mutate(bioguide_id = data$bioguide) %>%
      select(bioguide_id, date, party, any_of(c("text", "word_count")))
  }, error = function(e) NULL)
}, .progress = TRUE)

set.seed(1234)
cs_sample <- cs_index %>%
  filter(party %in% c("Democrat", "Republican")) %>%
  mutate(
    date     = as.Date(date),
    year     = as.integer(format(date, "%Y")),
    congress = 83 + floor((year - 1953) / 2)
  ) %>%
  filter(congress >= 104, congress <= 118, nchar(text) > 300) %>%
  group_by(congress, party) %>%
  slice_sample(n = 2000) %>%
  ungroup() %>%
  select(congress, party, text)

rm(cs_index); gc()

write_rds(cs_sample, "data/processed/conspeak_fw_sample.rds")

# PART 3: Build FW dictionary
# Monroe, Colaresi & Quinn (2008) standardized log odds ratio

combined_sample <- bind_rows(
  gs_sample %>% select(congress, party, text),
  cs_sample %>% select(congress, party, text)
)

rm(gs_sample, cs_sample); gc()

chunk_size <- 10000
n_chunks   <- ceiling(nrow(combined_sample) / chunk_size)

processed <- map_df(1:n_chunks, function(i) {
  start <- (i - 1) * chunk_size + 1
  end   <- min(i * chunk_size, nrow(combined_sample))
  combined_sample[start:end, ] %>%
    mutate(
      text_clean   = map_chr(text, clean_text),
      text_stemmed = map_chr(text_clean, stem_words)
    ) %>%
    select(congress, party, text_stemmed)
})

rm(combined_sample); gc()

bigrams <- processed %>%
  unnest_tokens(bigram, text_stemmed, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

rm(processed); gc()

bigram_counts <- bigrams %>%
  group_by(party, bigram) %>%
  summarise(n = n(), .groups = "drop")

rm(bigrams); gc()

bigram_wide <- bigram_counts %>%
  pivot_wider(names_from = party, values_from = n, values_fill = 0) %>%
  rename(n_dem = Democrat, n_rep = Republican) %>%
  filter(n_dem >= 10, n_rep >= 10, n_dem + n_rep >= 100)

rm(bigram_counts); gc()

alpha <- 1
N_dem <- sum(bigram_wide$n_dem)
N_rep <- sum(bigram_wide$n_rep)

fw_scores <- bigram_wide %>%
  mutate(
    pi_dem   = (n_dem + alpha) / (N_dem + nrow(bigram_wide) * alpha),
    pi_rep   = (n_rep + alpha) / (N_rep + nrow(bigram_wide) * alpha),
    log_odds = log(pi_dem / (1 - pi_dem)) - log(pi_rep / (1 - pi_rep)),
    variance = 1 / (n_dem + alpha) + 1 / (n_rep + alpha),
    fw_score = log_odds / sqrt(variance)
  ) %>%
  arrange(desc(fw_score))

rm(bigram_wide); gc()

stop_words_single <- c(
  "the", "a", "an", "of", "in", "to", "and", "or", "for", "by", "on",
  "at", "from", "with", "as", "is", "are", "was", "were", "be", "been",
  "have", "has", "had", "do", "does", "did", "will", "would", "shall",
  "should", "may", "might", "must", "can", "could", "that", "this",
  "these", "those", "it", "its", "we", "our", "they", "their", "he",
  "she", "his", "her", "not", "no", "but", "if", "when", "than", "then",
  "so", "up", "out", "about", "into", "also", "just", "new", "ii",
  "iii", "iv"
)

remove_patterns <- c(
  "biden", "obama", "trump", "clinton", "bush",
  "republican", "democrat", "colleagu",
  "remain avail", "avail until", "provid further",
  "air carrier", "unman aircraft", "tobacco product",
  "eagl scout", "rhode island", "nativ hawaiian", "heritag area"
)

fw_final <- fw_scores %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    !word1 %in% stop_words_single,
    !word2 %in% stop_words_single,
    !str_detect(bigram, "\\d"),
    str_length(word1) > 2,
    str_length(word2) > 2,
    !str_detect(bigram, "^(administr|insert|strike|subchapt|titl|chapt|section)"),
    !str_detect(bigram, paste(remove_patterns, collapse = "|"))
  ) %>%
  select(-word1, -word2)

write_rds(fw_final, "data/processed/fw_dictionary.rds")

# PART 4: Score primary loser speeches

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds")

chunk_size <- 5000
n_chunks   <- ceiling(nrow(analysis_data) / chunk_size)

processed <- map_df(1:n_chunks, function(i) {
  start <- (i - 1) * chunk_size + 1
  end   <- min(i * chunk_size, nrow(analysis_data))
  analysis_data[start:end, ] %>%
    mutate(
      text_clean   = map_chr(text, clean_text),
      text_stemmed = map_chr(text_clean, stem_words)
    ) %>%
    select(bioguide, Name, Year, party, post_primary, word_count, text_stemmed)
})

rm(analysis_data); gc()

chunk_size <- 2000
n_chunks   <- ceiling(nrow(processed) / chunk_size)

fw_speech_scores <- map_df(1:n_chunks, function(i) {
  start <- (i - 1) * chunk_size + 1
  end   <- min(i * chunk_size, nrow(processed))
  processed[start:end, ] %>%
    mutate(fw_score = map_dbl(text_stemmed, ~score_fw(.x, fw_final))) %>%
    select(bioguide, Name, Year, party, post_primary, word_count, fw_score)
})

rm(processed); gc()

# PART 5: Aggregate to member-period level

fw <- fw_speech_scores %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    n_speeches    = n(),
    mean_fw_score = mean(fw_score, na.rm = TRUE),
    pct_zero      = mean(fw_score == 0, na.rm = TRUE),
    .groups       = "drop"
  )

# PART 6: Validation — party discrimination pre-primary

fw_pre   <- fw %>% filter(post_primary == FALSE,
                          party %in% c("Democrat", "Republican"))
t_result <- t.test(mean_fw_score ~ party, data = fw_pre)

fw_pre %>%
  group_by(party) %>%
  summarise(mean = round(mean(mean_fw_score, na.rm = TRUE), 3), n = n()) %>%
  print()

cat("Party discrimination p-value:", round(t_result$p.value, 4), "\n")

write_rds(fw, "data/processed/fw.rds")