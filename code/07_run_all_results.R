# 07_run_all_results.R
# Loads all measure files and covariates, runs all DiD regressions,
# heterogeneity analyses, and robustness checks.
# All specifications use member FE + party x year FE + NOMINATE, HC1 SEs.
# Output: printed results with Ns

library(tidyverse)
library(fixest)
library(readxl)


# PART 1: Load and merge all measures with covariates

covariates <- read_rds("data/processed/covariates.rds")

merge_covariates <- function(df) {
  df %>%
    left_join(covariates, by = c("bioguide", "Year")) %>%
    group_by(bioguide) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    filter(!is.na(nominate_dim1)) %>%
    mutate(party_factor = factor(party))
}

fw        <- read_rds("data/processed/fw.rds")        %>% merge_covariates()
custom    <- read_rds("data/processed/custom.rds")    %>% merge_covariates()
certainty <- read_rds("data/processed/certainty.rds") %>% merge_covariates()
focus     <- read_rds("data/processed/focus.rds")     %>% merge_covariates()

cat("Sample sizes after merging covariates:\n")
cat("  FW:        ", n_distinct(fw$bioguide),        "members,", nrow(fw),        "obs\n")
cat("  Custom:    ", n_distinct(custom$bioguide),     "members,", nrow(custom),    "obs\n")
cat("  Certainty: ", n_distinct(certainty$bioguide),  "members,", nrow(certainty), "obs\n")
cat("  Focus:     ", n_distinct(focus$bioguide),      "members,", nrow(focus),     "obs\n")

# TABLE 1 SUMMARY STATISTICS

primary_losers <- read_excel("data/raw/primary_losers.xlsx") %>%
  mutate(Primary_Date = as.Date(Primary_Date))

data     <- read_rds("data/processed/final_analysis_data_CLEAN.rds")
coverage <- read_csv("data/processed/coverage_summary.csv", show_col_types = FALSE)

# --- Panel A ---
fw_all <- read_rds("data/processed/fw.rds")
cat("Democrat (%):", round(mean(fw_all$party == "Democrat", na.rm = TRUE) * 100, 1), "  N =", n_distinct(fw_all$bioguide), "\n")
cat("Pre-1994 (%):", round(mean(primary_losers$Year < 1995, na.rm = TRUE) * 100, 1), "  N =", nrow(primary_losers), "\n")

cat("NOMINATE mean:", round(mean(covariates$nominate_dim1, na.rm = TRUE), 3), "\n")
cat("NOMINATE SD:  ", round(sd(covariates$nominate_dim1,   na.rm = TRUE), 3), "\n")
cat("NOMINATE min: ", round(min(covariates$nominate_dim1,  na.rm = TRUE), 3), "\n")
cat("NOMINATE max: ", round(max(covariates$nominate_dim1,  na.rm = TRUE), 3), "\n")
cat("NOMINATE N:   ", sum(!is.na(covariates$nominate_dim1)), "\n")

cat("Margin mean:  ", round(mean(covariates$margin, na.rm = TRUE), 1), "\n")
cat("Margin min:   ", round(min(covariates$margin,  na.rm = TRUE), 1), "\n")
cat("Margin max:   ", round(max(covariates$margin,  na.rm = TRUE), 1), "\n")
cat("Margin N:     ", sum(!is.na(covariates$margin)), "\n")

# --- Panel B ---
n_pre_members  <- sum(coverage$pre_speeches  > 0)
n_post_members <- sum(coverage$post_speeches > 0)
n_both         <- sum(coverage$has_both)
total_pre      <- sum(coverage$pre_speeches)
total_post     <- sum(coverage$post_speeches)

cat("Total members with speech data:    ", n_distinct(data$bioguide), "\n")
cat("Members with both pre+post (DiD):  ", n_both, "\n")
cat("Total speeches (post-60w filter):  ", nrow(data), "\n")
cat("Total pre-primary speeches:        ", total_pre,  "  N members =", n_pre_members,  "\n")
cat("Total post-primary speeches:       ", total_post, "  N members =", n_post_members, "\n")
cat("Mean speeches per member (pre):    ", round(total_pre / n_pre_members, 1), "\n")

# --- Panel C ---
for (lst in list(
  list(df = read_rds("data/processed/fw.rds")        %>% filter(post_primary == FALSE),
       col = "mean_fw_score",       label = "FW Partisan Tenor"),
  list(df = read_rds("data/processed/custom.rds")    %>% filter(post_primary == FALSE),
       col = "mean_custom_score",   label = "Custom Partisan Tenor"),
  list(df = read_rds("data/processed/certainty.rds") %>% filter(post_primary == FALSE),
       col = "mean_certainty_score",label = "Certainty/Hedging"),
  list(df = read_rds("data/processed/focus.rds")     %>% filter(post_primary == FALSE),
       col = "mean_focus_score",    label = "Constituency Focus")
)) {
  x <- lst$df[[lst$col]]
  cat(sprintf("%-30s mean = %7.3f  SD = %6.3f  min = %7.3f  max = %7.3f  N = %d\n",
              lst$label,
              mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
              min(x, na.rm = TRUE),  max(x, na.rm = TRUE),
              sum(!is.na(x))))
}

# PART 2: Helper functions

# Standard full spec
run_did <- function(df, outcome, label) {
  f <- as.formula(paste(outcome,
                        "~ post_primary + nominate_dim1 | bioguide + party_factor^Year"))
  m <- feols(f, data = df, vcov = "HC1")
  pp <- grep("^post_primary", names(coef(m)), value = TRUE)[1]
  cat(sprintf("%-30s β = %7.3f  SE = %6.3f  p = %5.3f  N = %d\n",
              label,
              coef(m)[pp], se(m)[pp], pvalue(m)[pp], nobs(m)))
  invisible(m)
}

# Subgroup split
run_split <- function(df, outcome, split_var, split_val, label) {
  d <- df %>% filter(!is.na(.data[[split_var]]))
  d_sub <- if (is.logical(df[[split_var]])) {
    d %>% filter(.data[[split_var]] == split_val)
  } else {
    d %>% filter(.data[[split_var]] == split_val)
  }
  if (n_distinct(d_sub$bioguide) < 5) {
    cat(sprintf("%-30s insufficient N\n", label))
    return(invisible(NULL))
  }
  f <- as.formula(paste(outcome,
                        "~ post_primary + nominate_dim1 | bioguide + party_factor^Year"))
  m <- feols(f, data = d_sub, vcov = "HC1")
  pp <- grep("^post_primary", names(coef(m)), value = TRUE)[1]
  cat(sprintf("%-30s β = %7.3f  SE = %6.3f  p = %5.3f  N = %d  (n members = %d)\n",
              label,
              coef(m)[pp], se(m)[pp], pvalue(m)[pp], nobs(m),
              n_distinct(d_sub$bioguide)))
  invisible(m)
}


# PART 3: Main DiD results (Table 2)

m_fw        <- run_did(fw,        "mean_fw_score",        "FW Partisan Tenor")
m_custom    <- run_did(custom,    "mean_custom_score",    "Custom Partisan Tenor")
m_certainty <- run_did(certainty, "mean_certainty_score", "Certainty/Hedging")
m_focus     <- run_did(focus,     "mean_focus_score",     "Constituency Focus")

cat("\n-- All specs (member FE only / +party x year FE) --\n")

run_specs <- function(df, outcome, label) {
  f1 <- as.formula(paste(outcome, "~ post_primary | bioguide"))
  f3 <- as.formula(paste(outcome, "~ post_primary + nominate_dim1 | bioguide + party_factor^Year"))
  
  m1 <- feols(f1, data = df, vcov = "HC1")
  m3 <- feols(f3, data = df, vcov = "HC1")
  
  for (m in list(m1, m3)) {
    pp <- grep("^post_primary", names(coef(m)), value = TRUE)[1]
    cat(sprintf("%-30s β = %7.3f  SE = %6.3f  p = %5.3f  N = %d\n",
                label, coef(m)[pp], se(m)[pp], pvalue(m)[pp], nobs(m)))
  }
  cat("\n")
}

cat("=== FW ===\n");        run_specs(fw,        "mean_fw_score",        "FW")
cat("=== Custom ===\n");    run_specs(custom,    "mean_custom_score",    "Custom")
cat("=== Certainty ===\n"); run_specs(certainty, "mean_certainty_score", "Certainty")
cat("=== Focus ===\n");     run_specs(focus,     "mean_focus_score",     "Focus")

# PART 4: Era split (Table 3)

for (measure in list(
  list(df = fw,        outcome = "mean_fw_score",        label = "FW"),
  list(df = custom,    outcome = "mean_custom_score",    label = "Custom"),
  list(df = certainty, outcome = "mean_certainty_score", label = "Certainty"),
  list(df = focus,     outcome = "mean_focus_score",     label = "Focus")
)) {
  cat("\n--", measure$label, "--\n")
  run_split(measure$df %>% mutate(era = Year >= 1995),
            measure$outcome, "era", FALSE, "Pre-1994")
  run_split(measure$df %>% mutate(era = Year >= 1995),
            measure$outcome, "era", TRUE,  "Post-1994")
}

# PART 5: Competitiveness splits (Table 3)

for (measure in list(
  list(df = fw,        outcome = "mean_fw_score",        label = "FW"),
  list(df = custom,    outcome = "mean_custom_score",    label = "Custom"),
  list(df = focus,     outcome = "mean_focus_score",     label = "Focus")
)) {
  cat("\n--", measure$label, "--\n")
  run_split(measure$df, measure$outcome, "competitive_pres",   TRUE,  "Competitive (pres)")
  run_split(measure$df, measure$outcome, "competitive_pres",   FALSE, "Safe (pres)")
  run_split(measure$df, measure$outcome, "competitive_lagged", TRUE,  "Competitive (lagged)")
  run_split(measure$df, measure$outcome, "competitive_lagged", FALSE, "Safe (lagged)")
}

# PART 6: Margin of loss (Table 3)

for (measure in list(
  list(df = fw,        outcome = "mean_fw_score",        label = "FW"),
  list(df = custom,    outcome = "mean_custom_score",    label = "Custom"),
  list(df = focus,     outcome = "mean_focus_score",     label = "Focus")
)) {
  cat("\n--", measure$label, "--\n")
  run_split(measure$df, measure$outcome, "close_loss", TRUE,  "Close (<10pp)")
  run_split(measure$df, measure$outcome, "close_loss", FALSE, "Blowout (>=10pp)")
}

cat("\nMargin sample sizes:\n")
fw %>% filter(!is.na(close_loss)) %>%
  group_by(close_loss) %>%
  summarise(n_members = n_distinct(bioguide), .groups = "drop") %>%
  print()

# PART 7: Future ambition (Table 3)

for (measure in list(
  list(df = fw,        outcome = "mean_fw_score",        label = "FW"),
  list(df = custom,    outcome = "mean_custom_score",    label = "Custom"),
  list(df = focus,     outcome = "mean_focus_score",     label = "Focus")
)) {
  cat("\n--", measure$label, "--\n")
  df_post <- measure$df %>% filter(Year >= 1995)
  run_split(df_post, measure$outcome, "ambitious", TRUE,  "Ambitious")
  run_split(df_post, measure$outcome, "ambitious", FALSE, "Exited")
}

cat("\nAmbition sample sizes:\n")
fw %>% filter(Year >= 1995, !is.na(ambitious)) %>%
  group_by(ambitious) %>%
  summarise(n_members = n_distinct(bioguide), .groups = "drop") %>%
  print()

# PART 8: Post-treatment selection robustness check (FW only)

fw_pre_members  <- fw %>% filter(post_primary == FALSE) %>%
  distinct(bioguide, Name, Year, party)
fw_post_members <- fw %>% filter(post_primary == TRUE) %>%
  distinct(bioguide)

zero_rows <- fw_pre_members %>%
  filter(!bioguide %in% fw_post_members$bioguide) %>%
  left_join(covariates %>% select(bioguide, Year, nominate_dim1),
            by = c("bioguide", "Year")) %>%
  mutate(post_primary  = TRUE,
         mean_fw_score = 0,
         n_speeches    = 0,
         party_factor  = factor(party))

fw_robust <- bind_rows(
  fw %>% select(bioguide, Name, Year, party, party_factor,
                post_primary, mean_fw_score, nominate_dim1),
  zero_rows %>% select(bioguide, Name, Year, party, party_factor,
                       post_primary, mean_fw_score, nominate_dim1)
)

cat("\nZero-imputed members added:", nrow(zero_rows), "\n")
run_did(fw_robust, "mean_fw_score", "FW (zero imputed)")

# PART 9: Validation summary

fw_pre_val <- read_rds("data/processed/fw.rds") %>%
  filter(post_primary == FALSE, party %in% c("Democrat", "Republican"))
t_fw <- t.test(mean_fw_score ~ party, data = fw_pre_val)
fw_pre_val %>%
  group_by(party) %>%
  summarise(mean = round(mean(mean_fw_score, na.rm = TRUE), 3), n = n()) %>%
  print()
cat("FW p-value:", round(t_fw$p.value, 6), "\n")

custom_pre_val <- read_rds("data/processed/custom.rds") %>%
  filter(post_primary == FALSE, party %in% c("Democrat", "Republican"))
t_custom <- t.test(mean_custom_score ~ party, data = custom_pre_val)
custom_pre_val %>%
  group_by(party) %>%
  summarise(mean = round(mean(mean_custom_score, na.rm = TRUE), 3), n = n()) %>%
  print()
cat("Custom p-value:", round(t_custom$p.value, 6), "\n")