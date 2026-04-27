# Please note: This script was used to generate primary_losers_pres_comp_ALL.csv,
# which is provided in data/raw/. It does not need to be rerun.

# Complete Historical Presidential Vote Share Analysis (1952-2020)
# Processes ALL presidential elections from 1952-2020
# Aggregates county-level to district-level using appropriate Census crosswalks

library(tidyverse)
library(readxl)
library(fixest)


# STEP 1: Define Function

aggregate_year <- function(year, congress_num, census_year, leip_filepath, leip_sheetname = "County") {
  
  message("\n--- Processing ", year, " (Congress ", congress_num, ", ", census_year, " Census) ---")
  
  # Load crosswalk for this Congress
  crosswalk_file <- paste0("~/Downloads/CountyToCD-DOC/Crosswalk_", census_year, "_", congress_num, ".csv")
  
  if (!file.exists(crosswalk_file)) {
    message("  WARNING: Crosswalk file not found: ", crosswalk_file)
    return(NULL)
  }
  
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE)
  message("  Loaded crosswalk: ", nrow(crosswalk), " mappings")
  
  # Load Leip data
  if (!file.exists(leip_filepath)) {
    message("  WARNING: Leip file not found: ", leip_filepath)
    return(NULL)
  }
  
  leip_raw <- read_excel(leip_filepath, sheet = leip_sheetname)
  message("  Loaded Leip data: ", nrow(leip_raw), " counties")
  
  # Clean Leip data (column names vary by year)
  # Use positions instead of names since names vary
  leip_clean <- leip_raw %>%
    filter(!is.na(.data[[names(.)[2]]])) %>%  # Filter where column 2 is not NA
    select(
      county_name = 1,      # First column
      state_abbr = 2,       # Second column  
      total_votes = `Total Vote`,
      dem_pct = 10,         # Column 10 (Dem percentage)
      rep_pct = 11          # Column 11 (Rep percentage)
    ) %>%
    mutate(
      dem_votes = dem_pct * total_votes,
      rep_votes = rep_pct * total_votes
    )
  
  message("  Detected columns (positions 10, 11 for Dem/Rep)")
  message("  Cleaned: ", nrow(leip_clean), " counties")
  
  # State lookup
  state_lookup <- tribble(
    ~state_abbr, ~icpsrst,
    "AL", 41, "AK", 81, "AZ", 61, "AR", 42, "CA", 71, "CO", 62, "CT", 1, "DE", 11,
    "FL", 43, "GA", 44, "HI", 82, "ID", 63, "IL", 21, "IN", 22, "IA", 31, "KS", 32,
    "KY", 51, "LA", 45, "ME", 2, "MD", 52, "MA", 3, "MI", 23, "MN", 33, "MS", 46,
    "MO", 34, "MT", 64, "NE", 35, "NV", 65, "NH", 4, "NJ", 12, "NM", 66, "NY", 13,
    "NC", 47, "ND", 36, "OH", 24, "OK", 53, "OR", 72, "PA", 14, "RI", 5, "SC", 54,
    "SD", 37, "TN", 55, "TX", 49, "UT", 67, "VT", 6, "VA", 40, "WA", 73, "WV", 56,
    "WI", 25, "WY", 68, "DC", 15
  )
  
  # Match to FIPS
  leip_with_fips <- leip_clean %>%
    left_join(state_lookup, by = "state_abbr") %>%
    left_join(
      crosswalk %>% select(nhgisnam, icpsrst, icpsrfip) %>% distinct(),
      by = c("county_name" = "nhgisnam", "icpsrst" = "icpsrst")
    )
  
  match_rate <- mean(!is.na(leip_with_fips$icpsrfip))
  message("  County match rate: ", round(100 * match_rate, 1), "%")
  
  # Aggregate to districts
  district_aggregated <- leip_with_fips %>%
    filter(!is.na(icpsrfip)) %>%
    left_join(
      crosswalk %>% select(icpsrfip, congress, district, cd_state, m2_weight),
      by = "icpsrfip"
    ) %>%
    mutate(
      weighted_dem = dem_votes * m2_weight,
      weighted_rep = rep_votes * m2_weight,
      weighted_total = total_votes * m2_weight
    ) %>%
    group_by(cd_state, district) %>%
    summarise(
      total_dem = sum(weighted_dem, na.rm = TRUE),
      total_rep = sum(weighted_rep, na.rm = TRUE),
      total_votes = sum(weighted_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dem_share = total_dem / total_votes,
      year = year
    )
  
  message("  Aggregated: ", nrow(district_aggregated), " districts")
  
  return(district_aggregated)
}

# STEP 2: Define All Years to Process

# Map each presidential year to Congress and Census
years_to_process <- tribble(
  ~year, ~congress, ~census_year,
  # 2010s Census (2012-2020)
  2020, 116, 2010,
  2016, 114, 2010,
  2012, 112, 2010,
  # 2000s Census (2002-2010)
  2008, 110, 2000,
  2004, 108, 2000,
  2000, 106, 2000,
  # 1990s Census (1992-2000)
  1996, 104, 1990,
  1992, 102, 1990,
  # 1980s Census (1982-1990)
  1988, 100, 1980,
  1984, 98, 1980,
  1980, 96, 1980,
  # 1970s Census (1972-1980)
  1976, 94, 1970,
  1972, 92, 1970,
  # 1960s Census (1962-1970)
  1968, 90, 1960,
  1964, 88, 1960,
  1960, 86, 1960,
  # 1950s Census (1952-1960)
  1956, 84, 1950,
  1952, 82, 1950
) %>%
  mutate(leip_file = paste0("~/Downloads/Pres_Election_Data_", year, ".xlsx"))

message("Years to process: ", nrow(years_to_process))

# STEP 3: Process All Years

all_districts <- list()

for (i in 1:nrow(years_to_process)) {
  year_data <- years_to_process[i, ]
  
  result <- tryCatch({
    aggregate_year(
      year = year_data$year,
      congress_num = year_data$congress,
      census_year = year_data$census_year,
      leip_filepath = year_data$leip_file
    )
  }, error = function(e) {
    message("  ERROR: ", e$message)
    NULL
  })
  
  if (!is.null(result)) {
    all_districts[[as.character(year_data$year)]] <- result
  }
}

# Combine all years
presidential_by_district <- bind_rows(all_districts)

message("Years successfully processed: ", length(all_districts))
message("Total district-years: ", nrow(presidential_by_district))
message("Years covered: ", paste(sort(unique(presidential_by_district$year)), collapse = ", "))

# Save
write_csv(presidential_by_district, "~/Downloads/presidential_by_district_ALL.csv")

# STEP 4: Match to Primary Losers

primary_losers <- read_excel("~/Downloads/Primary Results/primary_losers.xlsx")

# Updated function for all years
get_lagged_pres_year <- function(primary_year) {
  case_when(
    primary_year >= 2021 ~ 2020,
    primary_year >= 2017 ~ 2016,
    primary_year >= 2013 ~ 2012,
    primary_year >= 2009 ~ 2008,
    primary_year >= 2005 ~ 2004,
    primary_year >= 2001 ~ 2000,
    primary_year >= 1997 ~ 1996,
    primary_year >= 1993 ~ 1992,
    primary_year >= 1989 ~ 1988,
    primary_year >= 1985 ~ 1984,
    primary_year >= 1981 ~ 1980,
    primary_year >= 1977 ~ 1976,
    primary_year >= 1973 ~ 1972,
    primary_year >= 1969 ~ 1968,
    primary_year >= 1965 ~ 1964,
    primary_year >= 1961 ~ 1960,
    primary_year >= 1957 ~ 1956,
    primary_year >= 1953 ~ 1952,
    TRUE ~ NA_real_
  )
}

# State abbreviation lookup
state_abbr_lookup <- tribble(
  ~State, ~cd_state,
  "AL", "Alabama", "AK", "Alaska", "AZ", "Arizona", "AR", "Arkansas",
  "CA", "California", "CO", "Colorado", "CT", "Connecticut", "DE", "Delaware",
  "FL", "Florida", "GA", "Georgia", "HI", "Hawaii", "ID", "Idaho",
  "IL", "Illinois", "IN", "Indiana", "IA", "Iowa", "KS", "Kansas",
  "KY", "Kentucky", "LA", "Louisiana", "ME", "Maine", "MD", "Maryland",
  "MA", "Massachusetts", "MI", "Michigan", "MN", "Minnesota", "MS", "Mississippi",
  "MO", "Missouri", "MT", "Montana", "NE", "Nebraska", "NV", "Nevada",
  "NH", "New Hampshire", "NJ", "New Jersey", "NM", "New Mexico", "NY", "New York",
  "NC", "North Carolina", "ND", "North Dakota", "OH", "Ohio", "OK", "Oklahoma",
  "OR", "Oregon", "PA", "Pennsylvania", "RI", "Rhode Island", "SC", "South Carolina",
  "SD", "South Dakota", "TN", "Tennessee", "TX", "Texas", "UT", "Utah",
  "VT", "Vermont", "VA", "Virginia", "WA", "Washington", "WV", "West Virginia",
  "WI", "Wisconsin", "WY", "Wyoming", "DC", "District of Columbia"
)

# Match
competitiveness_pres <- primary_losers %>%
  mutate(pres_year = get_lagged_pres_year(Year)) %>%
  left_join(state_abbr_lookup, by = "State") %>%
  left_join(
    presidential_by_district,
    by = c("pres_year" = "year", "cd_state" = "cd_state", "District" = "district")
  ) %>%
  competitive = case_when(
    is.na(dem_share) ~ NA,
    pmax(dem_share, 1 - dem_share) < 0.55 ~ TRUE,
    TRUE ~ FALSE
  )
    district_type = case_when(
      is.na(competitive) ~ "Unknown",
      competitive ~ "Competitive",
      TRUE ~ "Safe"
    )
  

match_rate <- mean(!is.na(competitiveness_pres$dem_share))
message("Match rate: ", round(100 * match_rate, 1), "%")
message("  Matched: ", sum(!is.na(competitiveness_pres$dem_share)))
message("  Unmatched: ", sum(is.na(competitiveness_pres$dem_share)))

message("\nCompetitiveness distribution:")
print(table(competitiveness_pres$district_type, useNA = "always"))

write_csv(competitiveness_pres, "~/Downloads/primary_losers_pres_comp_ALL.csv")


# STEP 5: Merge into Speech Data

comp_lookup <- read_csv("~/Downloads/primary_losers_pres_comp_ALL.csv",
                        show_col_types = FALSE) %>%
  select(bioguide = Bioguide_ID, Year, pres_dem_share = dem_share) %>%
  mutate(
    winner_share = pmax(pres_dem_share, 1 - pres_dem_share),
    competitive = case_when(
      is.na(pres_dem_share) ~ NA,
      winner_share < 0.55   ~ TRUE,
      TRUE                  ~ FALSE
    )
  )

partisan_comp <- read_rds("~/Downloads/partisan_scores_with_nominate.rds") %>%
  left_join(comp_lookup, by = c("bioguide", "Year"))

certainty_comp <- read_rds("~/Downloads/certainty_scores_with_nominate.rds") %>%
  left_join(comp_lookup, by = c("bioguide", "Year"))

focus_comp <- read_rds("~/Downloads/focus_scores_with_nominate.rds") %>%
  left_join(comp_lookup, by = c("bioguide", "Year"))

message("Merged presidential competitiveness:")
message("  Partisan: ", sum(!is.na(partisan_comp$competitive)), " with data")
message("  Certainty: ", sum(!is.na(certainty_comp$competitive)), " with data")
message("  Focus: ", sum(!is.na(focus_comp$competitive)), " with data")

# STEP 6: Run Regressions

run_competitiveness_split <- function(data, outcome_var, data_name) {
  message("\n--- ", data_name, " ---")
  
  reg_data <- data %>%
    filter(!is.na(competitive), !is.na(nominate_dim1)) %>%
    mutate(
      post = as.numeric(post_primary),
      party_factor = factor(party)
    )
  
  message("Total observations: ", nrow(reg_data))
  message("  Competitive districts: ", sum(reg_data$competitive))
  message("  Safe districts: ", sum(!reg_data$competitive))
  
  if (sum(reg_data$competitive) < 10 || sum(!reg_data$competitive) < 10) {
    message("  Skipping - insufficient observations (need 10+ in each group)")
    return(NULL)
  }
  
  formula_full <- as.formula(paste(outcome_var, "~ post + nominate_dim1 | bioguide + party_factor^Year"))
  
  spec_competitive <- feols(formula_full, data = reg_data %>% filter(competitive),
                            se = "cluster", cluster = ~bioguide)
  
  spec_safe <- feols(formula_full, data = reg_data %>% filter(!competitive),
                     se = "cluster", cluster = ~bioguide)
  
  message("\nCOMPETITIVE DISTRICTS (pres 45-55%):")
  print(summary(spec_competitive))
  
  message("\nSAFE DISTRICTS (pres <45% or >55%):")
  print(summary(spec_safe))
  
  extract_coef <- function(model, label) {
    if (is.null(model)) return(NULL)
    coef_val  <- coef(model)["post"]
    se_val    <- se(model)["post"]
    coef_table <- coeftable(model)
    p_val     <- coef_table["post", "Pr(>|t|)"]
    n_obs     <- nobs(model)
    
    tibble(
      District_Type = label,
      Coefficient   = round(coef_val, 4),
      Std_Error     = round(se_val, 4),
      p_value       = round(p_val, 4),
      Significant   = ifelse(p_val < 0.05, "Yes", "No"),
      N             = n_obs
    )
  }
  
  bind_rows(
    extract_coef(spec_competitive, "Competitive"),
    extract_coef(spec_safe, "Safe")
  )
}

partisan_results <- run_competitiveness_split(
  partisan_comp,
  "mean_partisan_score",
  "PARTISAN TENOR"
)

certainty_results <- run_competitiveness_split(
  certainty_comp,
  "mean_certainty_score",
  "CERTAINTY/HEDGING"
)

focus_results <- run_competitiveness_split(
  focus_comp,
  "mean_focus_score",
  "CONSTITUENCY VS POLICY FOCUS"
)

# STEP 7: Save Results

if (!is.null(partisan_results)) {
  write_csv(partisan_results, "~/Downloads/pres_comp_ALL_partisan.csv")
}
if (!is.null(certainty_results)) {
  write_csv(certainty_results, "~/Downloads/pres_comp_ALL_certainty.csv")
}
if (!is.null(focus_results)) {
  write_csv(focus_results, "~/Downloads/pres_comp_ALL_focus.csv")
}

# STEP 8: Final Summary

message("PRESIDENTIAL COMPETITIVENESS (Complete 1952-2020):")
message("\nPARTISAN TENOR:")
if (!is.null(partisan_results)) print(partisan_results)

message("\nCERTAINTY:")
if (!is.null(certainty_results)) print(certainty_results)

message("\nCONSTITUENCY FOCUS:")
if (!is.null(focus_results)) print(focus_results)