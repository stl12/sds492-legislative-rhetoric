# main.R
# Replication code for:
# "Electoral Incentives and Legislative Rhetoric:
#  Evidence from Primary Losers in the U.S. House, 1954-2024"
# Sarah Lepkowitz, Yale University, 2026

# Please note: Scripts 02 and 05 are computationally intensive (~several hours each).
# Pre-computed outputs are provided in data/processed/ so they do not need to
# be rerun. To skip them, start from script 06.

# Please note: Scripts 01, 02, and 05 require two speech corpora not included in
# this repository. See README for download instructions.

# Data construction
source("code/01_data_merge.R")
source("code/02_fw_scores.R")        # slow: ~few hours
source("code/03_custom_scores.R")
source("code/04_certainty_scores.R")
source("code/05_focus_scores.R")     # slow: ~few hours (keyATM)
source("code/06_covariates.R")

# Analysis
source("code/07_run_all_results.R")
source("code/08_speech_volume.R")
source("code/09_event_study.R")

# Figures
source("code/10_figures.R")