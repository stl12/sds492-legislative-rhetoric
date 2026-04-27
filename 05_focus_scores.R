# 05_focus_scores.R
# Runs keyATM topic model on primary loser speeches.
# Classifies 20 PAP topics into 7 constituency and 13 policy topics.
# Outcome = mean policy prevalence minus mean constituency prevalence.
# Output: focus.rds, keyatm_model.rds
# Please note: This script is computationally intensive (~several hours).
# Pre-computed outputs (focus.rds, keyatm_model.rds) are provided in data/processed/.
# To skip rerunning, proceed directly to 06_covariates.R.

library(tidyverse)
library(tidytext)
library(keyATM)
library(quanteda)

# Load data and build document-term matrix

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds") %>%
  mutate(speech_id = row_number())

speech_word_counts <- analysis_data %>%
  select(speech_id, text) %>%
  unnest_tokens(word, text) %>%
  filter(
    !word %in% stop_words$word,
    str_length(word) > 2,
    !str_detect(word, "\\d")
  ) %>%
  count(speech_id, word) %>%
  group_by(word) %>%
  filter(n_distinct(speech_id) >= 5) %>%
  ungroup()

speech_dfm  <- speech_word_counts %>% cast_dfm(speech_id, word, n)
keyatm_docs <- keyATM_read(texts = speech_dfm)

# PAP keywords (adapted from Fu 2024, Table A.4)
# Constituency topics marked with *

keywords <- list(
  # Constituency topics
  Agriculture      = c("farmer", "farmers", "farm", "crop", "crops",
                       "rural", "agriculture", "agricultural", "livestock",
                       "dairy", "grain", "fishing", "harvest", "drought",
                       "usda", "commodity", "irrigation"),
  Transportation   = c("infrastructure", "highway", "road", "roads", "rail",
                       "bridge", "bridges", "transit", "airport", "airports",
                       "transportation", "traffic", "commute", "bus"),
  Housing          = c("housing", "homeless", "homeowner", "homeowners",
                       "neighborhood", "neighborhoods", "rent", "mortgage",
                       "mortgages", "affordable", "apartment", "landlord",
                       "eviction", "zoning"),
  Public_Lands     = c("park", "parks", "acres", "forest", "forests",
                       "wilderness", "conservation", "lands", "monument",
                       "refuge", "recreation", "scenic", "yellowstone"),
  Education        = c("school", "schools", "teacher", "teachers", "student",
                       "students", "classroom", "education", "college",
                       "university", "graduation", "learning", "math",
                       "reading", "campus"),
  Environment      = c("pollution", "pollute", "environmental", "emissions",
                       "climate", "toxic", "greenhouse", "contamination",
                       "cleanup", "watershed", "smog", "superfund",
                       "epa", "carbon", "recycling"),
  Law_Crime        = c("crime", "crimes", "criminal", "police", "violence",
                       "violent", "safety", "drug", "drugs", "gang",
                       "prison", "jail", "victim", "victims",
                       "neighborhood", "community"),
  
  # Policy topics
  Macroeconomics   = c("economy", "economic", "deficit", "jobs", "spending",
                       "growth", "pay", "invest", "investment", "billion",
                       "money", "recovery", "credit", "unemployment",
                       "recession"),
  Civil_Rights     = c("discrimination", "marriage", "race", "racial",
                       "human", "rights", "equality", "black", "white",
                       "minority", "civil", "justice", "freedom",
                       "voting", "gender"),
  Health           = c("medicare", "medicaid", "medical", "drug", "drugs",
                       "doctor", "patient", "patients", "prescription",
                       "research", "cancer", "hospital", "insurance",
                       "healthcare", "opioid"),
  Labor            = c("worker", "workers", "wage", "wages", "training",
                       "skill", "skills", "unemployed", "unemployment",
                       "union", "pension", "workplace", "earning",
                       "minimum", "salary"),
  Energy           = c("energy", "gas", "fuel", "oil", "solar", "wind",
                       "nuclear", "electricity", "power", "renewable",
                       "efficient", "efficiency", "carbon", "pipeline",
                       "grid"),
  Immigration      = c("border", "illegal", "immigration", "immigrant",
                       "immigrants", "alien", "amnesty", "deportation",
                       "citizenship", "visa", "refugee", "asylum",
                       "undocumented", "pathway", "dream"),
  Social_Welfare   = c("poverty", "poor", "welfare", "medicaid",
                       "nutrition", "hunger", "assistance", "earned",
                       "snap", "supplemental", "tanf", "subsidized",
                       "low-income", "destitute", "impoverished"),
  Domestic_Commerce = c("credit", "invest", "investment", "mortgage",
                        "entrepreneur", "entrepreneurs", "crisis", "lending",
                        "bank", "banks", "financial", "business",
                        "commerce", "market", "regulation"),
  Defense          = c("nuclear", "weapon", "weapons", "troops", "military",
                       "allies", "terrorist", "terrorism", "nato", "iraq",
                       "iran", "army", "pentagon", "warfare", "combat"),
  Technology       = c("research", "space", "technology", "internet",
                       "computer", "computers", "innovation", "digital",
                       "science", "scientific", "tech", "artificial",
                       "cyber", "data", "broadband"),
  Foreign_Trade    = c("trade", "china", "export", "exports", "import",
                       "imports", "tariff", "tariffs", "agreement",
                       "partner", "competition", "dumping", "wto",
                       "nafta", "outsourcing"),
  Intl_Affairs     = c("terrorism", "democracy", "democratic", "weapon",
                       "terror", "east", "allies", "afghanistan", "threat",
                       "regime", "regimes", "sanctions", "diplomacy",
                       "foreign", "international"),
  Govt_Operations  = c("bipartisan", "lobbyist", "bureaucracy",
                       "oversight", "accountability", "transparency",
                       "ethics", "corruption", "watchdog", "inspector")
)

constituency_topics <- c("Agriculture", "Transportation", "Housing",
                         "Public_Lands", "Education", "Environment",
                         "Law_Crime")

policy_topics <- c("Macroeconomics", "Civil_Rights", "Health", "Labor",
                   "Energy", "Immigration", "Social_Welfare",
                   "Domestic_Commerce", "Defense", "Technology",
                   "Foreign_Trade", "Intl_Affairs", "Govt_Operations")

# Run keyATM
# seed = 1234, 1500 iterations for convergence

set.seed(1234)

keyatm_model <- keyATM(
  docs              = keyatm_docs,
  no_keyword_topics = 3,
  keywords          = keywords,
  model             = "base",
  options           = list(seed = 1234, iterations = 1500, verbose = FALSE)
)

saveRDS(keyatm_model, "data/processed/keyatm_model.rds")

# Extract topic proportions and compute focus score

topic_names <- c(names(keywords), paste0("Other_", 1:3))
speech_ids  <- as.numeric(docnames(speech_dfm))

theta_df <- as_tibble(keyatm_model$theta) %>%
  setNames(topic_names) %>%
  mutate(speech_id = speech_ids)

focus_data <- analysis_data %>%
  select(speech_id, bioguide, Name, Year, post_primary, party) %>%
  inner_join(theta_df, by = "speech_id") %>%
  mutate(
    constituency_score = rowSums(select(., all_of(constituency_topics))),
    policy_score       = rowSums(select(., all_of(policy_topics))),
    focus_score        = policy_score - constituency_score
    # Positive = more policy focused, negative = more constituency focused
  )

# Aggregate to member-period level and save

focus <- focus_data %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    n_speeches        = n(),
    mean_focus_score  = mean(focus_score, na.rm = TRUE),
    mean_constituency = mean(constituency_score, na.rm = TRUE),
    mean_policy       = mean(policy_score, na.rm = TRUE),
    .groups           = "drop"
  )

write_rds(focus, "data/processed/focus.rds")