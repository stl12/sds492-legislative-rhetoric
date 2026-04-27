# 08_speech_volume.R
# Replicates Kim (2025) speech volume finding.
# Unbalanced window: full congressional session.
# Balanced window: pre-primary window = post-primary window length.
# Output: printed results

library(tidyverse)
library(fixest)
library(readxl)

# Load data

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds")

covariates <- read_rds("data/processed/covariates.rds") %>%
  select(bioguide, Year, nominate_dim1)

# PART 1: Unbalanced window (full session)

member_volume <- analysis_data %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    total_words = sum(word_count, na.rm = TRUE),
    n_speeches  = n(),
    .groups     = "drop"
  ) %>%
  mutate(log_words = log(total_words + 1)) %>%
  group_by(bioguide) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  left_join(covariates, by = c("bioguide", "Year")) %>%
  mutate(party_factor = factor(party))

vol_full <- feols(log_words ~ post_primary + nominate_dim1 |
                    bioguide + party_factor^Year,
                  data = member_volume, vcov = "HC1")

pp <- grep("^post_primary", names(coef(vol_full)), value = TRUE)[1]
cat("Unbalanced window:\n")
cat(sprintf("  β = %.3f  SE = %.3f  p = %.3f  N = %d\n",
            coef(vol_full)[pp], se(vol_full)[pp],
            pvalue(vol_full)[pp], nobs(vol_full)))
cat(sprintf("  Proportional change: %.1f%%\n",
            (exp(coef(vol_full)[pp]) - 1) * 100))

# PART 2: Balanced window (Kim 2025)
# t* set so pre-primary window = post-primary window in length
# Uses exact congress_end dates from primary_losers.xlsx

windows <- read_excel("data/raw/primary_losers.xlsx") %>%
  mutate(
    Primary_Date = as.Date(Primary_Date),
    congress_end = as.Date(congress_end)
  ) %>%
  select(bioguide = Bioguide_ID, Year, Primary_Date, congress_end) %>%
  mutate(
    post_window_days = as.integer(congress_end - Primary_Date),
    reference_date   = Primary_Date - post_window_days
  ) %>%
  filter(post_window_days > 0)

final_balanced <- analysis_data %>%
  left_join(windows, by = c("bioguide", "Year")) %>%
  filter(!is.na(reference_date)) %>%
  mutate(
    Primary_Date = coalesce(Primary_Date.x, Primary_Date.y),
    congress_end = coalesce(congress_end.x, congress_end.y)
  ) %>%
  select(-Primary_Date.x, -Primary_Date.y,
         -congress_end.x,  -congress_end.y) %>%
  filter(
    (post_primary == FALSE & date >= reference_date & date <  Primary_Date) |
      (post_primary == TRUE  & date >= Primary_Date   & date <= congress_end)
  )

member_volume_bal <- final_balanced %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    total_words = sum(word_count, na.rm = TRUE),
    n_speeches  = n(),
    .groups     = "drop"
  ) %>%
  mutate(log_words = log(total_words + 1)) %>%
  group_by(bioguide) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  left_join(covariates, by = c("bioguide", "Year")) %>%
  mutate(party_factor = factor(party))

vol_bal <- feols(log_words ~ post_primary + nominate_dim1 |
                   bioguide + party_factor^Year,
                 data = member_volume_bal, vcov = "HC1")

pp <- grep("^post_primary", names(coef(vol_bal)), value = TRUE)[1]
cat("\nBalanced window (Kim 2025):\n")
cat(sprintf("  β = %.3f  SE = %.3f  p = %.3f  N = %d\n",
            coef(vol_bal)[pp], se(vol_bal)[pp],
            pvalue(vol_bal)[pp], nobs(vol_bal)))
cat(sprintf("  Proportional change: %.1f%%\n",
            (exp(coef(vol_bal)[pp]) - 1) * 100))