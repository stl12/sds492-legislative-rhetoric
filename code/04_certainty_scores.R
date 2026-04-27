# 04_certainty_scores.R
# Scores primary loser speeches using Loughran-McDonald certainty dictionary.
# Strong modals = +1 (certainty), uncertainty + weak modals = -1 (hedging).
# Output: certainty.rds

library(tidyverse)

# Load data

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds")

clean_text <- function(text) {
  if (is.na(text)) return("")
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  if (is.na(text)) return("")
  text %>%
    tolower() %>%
    str_replace_all("[^a-z0-9\\s'\\-]", " ") %>%
    str_squish()
}

analysis_data <- analysis_data %>%
  mutate(text_clean = map_chr(text, clean_text))

# Build certainty dictionary from Loughran-McDonald
# Strong modals = certainty (+1)
# Uncertainty + weak modals = hedging (-1)
# If a word appears in both, keep first occurrence (strong modal wins)

lm_dict <- read_csv("data/raw/Loughran-McDonald_MasterDictionary_1993-2024.csv",
                    show_col_types = FALSE)

certainty_dict <- bind_rows(
  lm_dict %>%
    filter(Strong_Modal > 0) %>%
    transmute(word = tolower(Word), certainty_score = 1),
  lm_dict %>%
    filter(Uncertainty > 0 | Weak_Modal > 0) %>%
    transmute(word = tolower(Word), certainty_score = -1)
) %>%
  distinct(word, .keep_all = TRUE)

# Score speeches

count_certainty_words <- function(text, dictionary) {
  if (is.na(text) || text == "") {
    return(tibble(certainty_score = 0))
  }
  words        <- str_split(text, "\\s+")[[1]]
  matched      <- dictionary %>% filter(word %in% words)
  if (nrow(matched) == 0) return(tibble(certainty_score = 0))
  word_counts  <- map_dbl(matched$word, ~sum(words == .x))
  tibble(certainty_score = sum(word_counts * matched$certainty_score))
}

certainty_scores <- analysis_data %>%
  rowwise() %>%
  mutate(scores = list(count_certainty_words(text_clean, certainty_dict))) %>%
  ungroup() %>%
  unnest(scores) %>%
  mutate(certainty_score_per1000 = (certainty_score / word_count) * 1000)

# Aggregate to member-period level and save

certainty <- certainty_scores %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    n_speeches          = n(),
    mean_certainty_score = mean(certainty_score_per1000, na.rm = TRUE),
    .groups             = "drop"
  )

write_rds(certainty, "data/processed/certainty.rds")