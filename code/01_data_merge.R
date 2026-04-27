# 01_data_merge.R
# Merges GS (1954-1994) + ConSpeak (1995-2024) with primary losers list.
# Uses exact congress start/end dates baked into primary_losers.xlsx.
# Output: final_analysis_data_CLEAN.rds, coverage_summary.csv

# Please note: This script requires two speech corpora not included in this repository:
# - Gentzkow-Shapiro corpus: https://data.stanford.edu/congress_text
# - ConSpeak corpus: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OOLQFQ
# Download and place in data/raw/hein-bound/ and data/raw/dataverse_files/ respectively.

library(tidyverse)
library(jsonlite)
library(readxl)

# Load primary losers
# congress, congress_start, congress_end already in xlsx

primary_losers <- read_excel("data/raw/primary_losers.xlsx") %>%
  mutate(
    Primary_Date   = as.Date(Primary_Date),
    congress_start = as.Date(congress_start),
    congress_end   = as.Date(congress_end),
    name_parts     = str_split(Name, " "),
    first_name     = map_chr(name_parts, ~.x[1]),
    last_name      = map_chr(name_parts, ~{
      parts <- .x
      parts <- parts[!parts %in% c("Jr.", "Sr.", "II", "III", "IV")]
      tail(parts, 1)
    })
  ) %>%
  select(-name_parts)

# Manual last name overrides for members with multi-word or hyphenated last names
primary_losers <- primary_losers %>%
  mutate(last_name = case_when(
    Bioguide_ID == "V000027" ~ "Vander Jagt",
    TRUE ~ last_name
  ))

# Get ConSpeak term dates for GS name disambiguation
# Members without a ConSpeak JSON get estimated dates from primary year

get_terms_from_conspeak <- function(bioguide_id) {
  file_path <- file.path("data/raw/dataverse_files",
                         paste0(bioguide_id, ".json"))
  if (!file.exists(file_path)) return(tibble())
  tryCatch({
    data <- fromJSON(file_path)
    if (is.null(data$terms) || length(data$terms) == 0) return(tibble())
    as_tibble(data$terms) %>%
      mutate(start_date = as.Date(start),
             end_date   = as.Date(end),
             chamber    = ifelse(type == "rep", "H", "S")) %>%
      select(start_date, end_date, chamber, state)
  }, error = function(e) tibble())
}

primary_losers_with_terms <- primary_losers %>%
  rowwise() %>%
  mutate(terms_data = list(get_terms_from_conspeak(Bioguide_ID))) %>%
  ungroup() %>%
  mutate(
    terms_data = map2(terms_data, Year, function(td, yr) {
      if (nrow(td) == 0) {
        tibble(start_date = as.Date(paste0(yr - 2, "-01-03")),
               end_date   = as.Date(paste0(yr,     "-12-31")),
               chamber    = "H",
               state      = NA_character_)
      } else td
    })
  ) %>%
  unnest(terms_data)

# Load GS speeches (83rd-103rd Congress)
# Two-strategy matching: exact (lastname+firstname+state+chamber), then lastname fallback

historical_lookup <- primary_losers_with_terms %>%
  filter(Year < 1995) %>%
  select(Bioguide_ID, first_name, last_name, State, chamber,
         start_date, end_date, congress_start, congress_end)

load_gs_congress <- function(congress_num, lookup_table,
                             data_dir = "data/raw/hein-bound") {
  congress_str    <- sprintf("%03d", congress_num)
  speeches_file   <- file.path(data_dir, paste0("speeches_",  congress_str, ".txt"))
  descr_file      <- file.path(data_dir, paste0("descr_",     congress_str, ".txt"))
  speakermap_file <- file.path(data_dir, paste0(congress_str, "_SpeakerMap.txt"))
  
  if (!all(file.exists(c(speeches_file, descr_file, speakermap_file)))) return(NULL)
  
  speeches   <- read_delim(speeches_file,   delim = "|",
                           col_names = c("speech_id", "text"),
                           col_types = cols(.default = "c"), show_col_types = FALSE)
  descr      <- read_delim(descr_file,      delim = "|",
                           col_types = cols(.default = "c"), show_col_types = FALSE)
  speakermap <- read_delim(speakermap_file, delim = "|",
                           col_types = cols(.default = "c"), show_col_types = FALSE)
  
  data <- speeches %>%
    left_join(descr,      by = "speech_id") %>%
    left_join(speakermap, by = "speech_id") %>%
    select(speech_id, speakerid, firstname, lastname, date,
           chamber = chamber.y, party, state = state.y, text) %>%
    mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
    filter(!is.na(speakerid))
  
  data_exact <- data %>%
    mutate(match_key = paste(str_replace_all(tolower(lastname), "[' ]", ""),
                             str_replace_all(tolower(firstname), "[' ]", ""),
                             state, chamber, sep = "_")) %>%
    inner_join(
      lookup_table %>%
        mutate(match_key = paste(str_replace_all(tolower(last_name), "[' ]", ""),
                                 str_replace_all(tolower(first_name), "[' ]", ""),
                                 State, chamber, sep = "_")),
      by = "match_key", relationship = "many-to-many"
    ) %>%
    rename(chamber = chamber.x) %>%
    filter(date >= start_date, date <= end_date) %>%
    group_by(speech_id) %>% slice(1) %>% ungroup()
  
  unmatched     <- setdiff(data$speech_id, data_exact$speech_id)
  data_fallback <- tibble()
  
  if (length(unmatched) > 0) {
    data_fallback <- data %>%
      filter(speech_id %in% unmatched) %>%
      mutate(lastname_key = paste(tolower(lastname), state, chamber, sep = "_")) %>%
      inner_join(
        lookup_table %>%
          mutate(lastname_key = paste(tolower(last_name), State, chamber, sep = "_")),
        by = "lastname_key", relationship = "many-to-many"
      ) %>%
      rename(chamber = chamber.x) %>%
      filter(date >= start_date, date <= end_date) %>%
      group_by(speech_id) %>% slice(1) %>% ungroup()
  }
  
  bind_rows(
    data_exact    %>% mutate(match_type = "exact"),
    data_fallback %>% mutate(match_type = "fallback")
  ) %>%
    select(speech_id, date, chamber, party, state, text,
           bioguide = Bioguide_ID, match_type,
           congress_start, congress_end) %>%
    mutate(source = "Gentzkow-Shapiro")
}

gs_speeches <- map_df(83:103, function(cong) {
  tryCatch(load_gs_congress(cong, historical_lookup), error = function(e) NULL)
})

# Who in the historical sample is unmatched?
historical_unmatched <- primary_losers %>%
  filter(Year < 1995) %>%
  filter(!Bioguide_ID %in% gs_speeches$bioguide) %>%
  select(Bioguide_ID, Name, Year, State)

print(historical_unmatched)

# Load ConSpeak speeches (104th-118th Congress)

load_conspeak <- function(bioguide_id, data_dir = "data/raw/dataverse_files") {
  file_path <- file.path(data_dir, paste0(bioguide_id, ".json"))
  if (!file.exists(file_path)) return(NULL)
  tryCatch({
    data <- fromJSON(file_path)
    as_tibble(data$speeches) %>%
      mutate(bioguide   = bioguide_id,
             date       = as.Date(date),
             source     = "ConSpeak",
             match_type = "bioguide")
  }, error = function(e) NULL)
}

conspeak_speeches <- map_df(
  primary_losers %>% filter(Year >= 1995) %>% pull(Bioguide_ID),
  load_conspeak
)

# Who in the modern sample is unmatched?
modern_unmatched <- primary_losers %>%
  filter(Year >= 1995) %>%
  filter(!Bioguide_ID %in% conspeak_speeches$bioguide) %>%
  select(Bioguide_ID, Name, Year, State)

print(modern_unmatched)

# Standardize and combine

gs_std <- gs_speeches %>%
  select(bioguide, date, chamber, party, state, text,
         source, match_type, congress_start, congress_end) %>%
  mutate(
    chamber = tolower(chamber),
    party   = case_when(
      party == "D" ~ "Democrat",
      party == "R" ~ "Republican",
      party == "I" ~ "Independent",
      TRUE         ~ party
    )
  )

cs_std <- conspeak_speeches %>%
  select(bioguide, date, chamber, party, text, source, match_type) %>%
  mutate(state          = NA_character_,
         congress_start = as.Date(NA),
         congress_end   = as.Date(NA))

# Merge with primary loser metadata
# Filter to exact congress window, deduplicate multi-cycle losers,
# compute post_primary fresh after all joins

analysis_data <- bind_rows(gs_std, cs_std) %>%
  left_join(
    primary_losers %>%
      select(Bioguide_ID, Name, Year, Primary_Date, State, District,
             congress, congress_start, congress_end),
    by = c("bioguide" = "Bioguide_ID"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    congress_start = coalesce(congress_start.x, congress_start.y),
    congress_end   = coalesce(congress_end.x,   congress_end.y)
  ) %>%
  select(-congress_start.x, -congress_start.y,
         -congress_end.x,   -congress_end.y) %>%
  filter(date >= congress_start, date < congress_end) %>%
  group_by(bioguide, date, text) %>%
  arrange(abs(date - Primary_Date)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    post_primary = date >= Primary_Date,
    post_primary = if_else(is.na(post_primary), FALSE, post_primary)
  ) %>%
  mutate(word_count = str_count(text, "\\w+")) %>%
  filter(word_count >= 60)

# Coverage summary and save

coverage <- analysis_data %>%
  group_by(bioguide, Name, Year) %>%
  summarise(
    pre_speeches  = sum(!post_primary),
    post_speeches = sum(post_primary),
    has_both      = (pre_speeches > 0) & (post_speeches > 0),
    .groups       = "drop"
  )

write_rds(analysis_data, "data/processed/final_analysis_data_CLEAN.rds")
write_csv(coverage,      "data/processed/coverage_summary.csv")