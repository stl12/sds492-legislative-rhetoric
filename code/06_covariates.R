# 06_covariates.R
# Builds a single member-level covariate file combining:
#   - NOMINATE ideology scores (VoteView)
#   - District competitiveness (presidential vote share, lagged general election)
#   - Margin of primary loss
#   - Future political ambition
# Output: covariates.rds

library(tidyverse)
library(readxl)

# NOMINATE

congress_lookup <- tibble(
  Year     = seq(1954, 2024, by = 2),
  congress = 83 + floor((Year - 1953) / 2)
)

nominate <- read_csv("data/raw/Hall_members.csv", show_col_types = FALSE) %>%
  select(bioguide = bioguide_id, congress, nominate_dim1) %>%
  filter(!is.na(nominate_dim1)) %>%
  group_by(bioguide, congress) %>%
  slice(1) %>%
  ungroup()

# Competitiveness: presidential vote share (lagged)
# Already computed and saved — read directly

comp_pres <- read_csv("data/raw/primary_losers_pres_comp_ALL.csv",
                      show_col_types = FALSE) %>%
  select(bioguide = Bioguide_ID, Year, dem_share) %>%
  mutate(
    winner_share     = pmax(dem_share, 1 - dem_share),
    competitive_pres = case_when(
      is.na(dem_share)       ~ NA,
      winner_share < 0.55    ~ TRUE,
      TRUE                   ~ FALSE
    )
  ) %>%
  select(bioguide, Year, dem_share, competitive_pres)

# Competitiveness: lagged general election (MIT Election Lab)

primary_losers <- read_excel("data/raw/primary_losers.xlsx") %>%
  mutate(Primary_Date = as.Date(Primary_Date))

elections <- read_csv("data/raw/1976-2024-house.tab",
                      show_col_types = FALSE) %>%
  filter(stage == "GEN", !is.na(candidatevotes)) %>%
  mutate(
    vote_share = candidatevotes / totalvotes,
    state      = state_po,
    district   = ifelse(is.na(district) | district == 0, 1, district)
  ) %>%
  group_by(year, state, district) %>%
  slice_max(vote_share, n = 1) %>%
  ungroup() %>%
  select(year, state, district, winner_vote_share = vote_share)

comp_lagged <- primary_losers %>%
  mutate(lagged_year = Year - 2) %>%
  left_join(elections,
            by = c("lagged_year" = "year",
                   "State"       = "state",
                   "District"    = "district")) %>%
  mutate(
    competitive_lagged = case_when(
      is.na(winner_vote_share)    ~ NA,
      winner_vote_share < 0.55   ~ TRUE,
      TRUE                        ~ FALSE
    )
  ) %>%
  select(bioguide = Bioguide_ID, Year,
         lagged_vote_share = winner_vote_share, competitive_lagged)

# Margin of primary loss

margin <- primary_losers %>%
  select(bioguide = Bioguide_ID, Year, margin = Margin) %>%
  mutate(close_loss = if_else(!is.na(margin), margin < 10, NA))

# Future ambition (post-1995 only)

ambition <- read_csv("data/raw/future_ambition_coding.csv",
                     show_col_types = FALSE) %>%
  filter(!is.na(ambition_code)) %>%
  mutate(ambitious = ambition_code %in% c(1, 2, 3)) %>%
  select(bioguide = Bioguide_ID, Year, ambition_code, ambitious, redistricted)

# Combine into single member-level covariate file and merge NOMINATE

covariates <- primary_losers %>%
  select(bioguide = Bioguide_ID, Year) %>%
  left_join(congress_lookup, by = "Year") %>%
  left_join(nominate,        by = c("bioguide", "congress")) %>%
  left_join(comp_pres,       by = c("bioguide", "Year")) %>%
  left_join(comp_lagged,     by = c("bioguide", "Year")) %>%
  left_join(margin,          by = c("bioguide", "Year")) %>%
  left_join(ambition,        by = c("bioguide", "Year"))

write_rds(covariates, "data/processed/covariates.rds")