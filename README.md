# Electoral Incentives and Legislative Rhetoric
## Replication Code — Sarah Lepkowitz, Yale University (2026)

This repository contains replication code for "Electoral Incentives and 
Legislative Rhetoric: Evidence from Primary Losers in the U.S. House, 1954-2024."

## Repository Structure

sds492-legislative-rhetoric/
├── main.R                        # Run all scripts in order
├── code/                         # Analysis scripts
├── data/
│   ├── raw/                      # Input data
│   └── processed/                # Pre-computed outputs
└── figures/                      # Output figures

## Code

- `00_appendix_pres_vote_share.R` — Preprocessing script that generated `primary_losers_pres_comp_ALL.csv` from county-level presidential returns. Pre-computed output is provided in `data/raw/`; this script does not need to be rerun.
- `01_data_merge.R` through `10_figures.R` — Main analysis pipeline. Run in order via `main.R`.

## Data

### Included in this repository (data/raw/)
- primary_losers.xlsx             — Primary loser sample with congress dates
- future_ambition_coding.csv      — Hand-coded future ambition (post-1995)
- Hall_members.csv                — NOMINATE ideology scores (VoteView)
- 1976-2024-house.tab             — MIT Election Lab House results
- primary_losers_pres_comp_ALL.csv — Presidential vote share competitiveness
- Loughran-McDonald_MasterDictionary_1993-2024.csv — LM dictionary

### Not included (too large or third-party terms of use)
Two speech corpora must be downloaded separately and placed in the indicated folders:

- **Gentzkow-Shapiro corpus** (1954-1994)
  Download: https://data.stanford.edu/congress_text
  Place in: data/raw/hein-bound/

- **ConSpeak corpus** (1995-2024)
  Download: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OOLQFQ
  Place in: data/raw/dataverse_files/

### Pre-computed outputs (data/processed/)
The following slow-to-generate files are provided so scripts 02 and 05
do not need to be rerun:
- final_analysis_data_CLEAN.rds (stored via Git LFS)
- fw.rds, fw_dictionary.rds
- custom.rds, certainty.rds, focus.rds
- keyatm_model.rds
- covariates.rds, coverage_summary.csv

## Running the Code

To replicate all results, run main.R from the repository root.
To skip the slow scripts, start from script 06:

source("code/06_covariates.R")

## Software

R version 4.x. Required packages: tidyverse, fixest, keyATM, quanteda,
tidytext, SnowballC, readxl, patchwork, maps, jsonlite.
