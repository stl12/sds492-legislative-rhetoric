# 03_custom_scores.R
# Scores primary loser speeches using 198-phrase hand-coded partisan dictionary.
# Positive scores = Republican phrases, negative = Democratic phrases.
# Output: custom.rds

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

# Partisan dictionary
# Positive = Republican, Negative = Democratic

partisan_dict <- tribble(
  ~phrase,                             ~party_score,
  
  # Republican phrases
  "tax relief",                         1,
  "tax cuts",                           1,
  "tax cut",                            1,
  "death tax",                          1,
  "tax increase",                       1,
  "tax increases",                      1,
  "job creators",                       1,
  "job creation",                       1,
  "free market",                        1,
  "free enterprise",                    1,
  "private sector",                     1,
  "economic growth",                    1,
  "pro growth",                         1,
  "capital gains",                      1,
  "small business",                     1,
  "small businesses",                   1,
  "entrepreneurship",                   1,
  "free trade",                         1,
  "big government",                     1,
  "government spending",                1,
  "wasteful spending",                  1,
  "out of control spending",            1,
  "government bureaucracy",             1,
  "red tape",                           1,
  "overregulation",                     1,
  "government takeover",                1,
  "government control",                 1,
  "nanny state",                        1,
  "fiscal responsibility",              1,
  "balanced budget",                    1,
  "spending cuts",                      1,
  "reduce spending",                    1,
  "limited government",                 1,
  "states rights",                      1,
  "tenth amendment",                    1,
  "right to life",                      1,
  "unborn child",                       1,
  "unborn children",                    1,
  "pro life",                           1,
  "sanctity of life",                   1,
  "traditional marriage",               1,
  "traditional values",                 1,
  "family values",                      1,
  "religious freedom",                  1,
  "religious liberty",                  1,
  "school choice",                      1,
  "parental rights",                    1,
  "law and order",                      1,
  "tough on crime",                     1,
  "law enforcement",                    1,
  "border security",                    1,
  "secure the border",                  1,
  "illegal immigration",                1,
  "illegal immigrants",                 1,
  "illegal alien",                      1,
  "illegal aliens",                     1,
  "amnesty",                            1,
  "national security",                  1,
  "strong defense",                     1,
  "military strength",                  1,
  "peace through strength",             1,
  "war on terror",                      1,
  "global war on terror",               1,
  "radical islam",                      1,
  "islamic terrorism",                  1,
  "second amendment",                   1,
  "right to bear arms",                 1,
  "gun rights",                         1,
  "law abiding gun owners",             1,
  "constitutional right",               1,
  "personal responsibility",            1,
  "individual freedom",                 1,
  "personal freedom",                   1,
  "liberty and freedom",                1,
  "self reliance",                      1,
  "government run healthcare",          1,
  "socialized medicine",                1,
  "patient choice",                     1,
  "health savings accounts",            1,
  "energy independence",                1,
  "american energy",                    1,
  "domestic energy",                    1,
  "drill baby drill",                   1,
  
  # Democratic phrases
  "working families",                  -1,
  "working family",                    -1,
  "middle class",                      -1,
  "income inequality",                 -1,
  "wealth inequality",                 -1,
  "fair share",                        -1,
  "tax breaks for the wealthy",        -1,
  "tax breaks for the rich",           -1,
  "tax cuts for the wealthy",          -1,
  "tax cuts for the rich",             -1,
  "corporate greed",                   -1,
  "corporate welfare",                 -1,
  "living wage",                       -1,
  "minimum wage",                      -1,
  "wage gap",                          -1,
  "pay equity",                        -1,
  "equal pay",                         -1,
  "economic justice",                  -1,
  "economic fairness",                 -1,
  "level playing field",               -1,
  "workers rights",                    -1,
  "labor unions",                      -1,
  "collective bargaining",             -1,
  "right to organize",                 -1,
  "union workers",                     -1,
  "labor standards",                   -1,
  "prevailing wage",                   -1,
  "social safety net",                 -1,
  "safety net",                        -1,
  "social programs",                   -1,
  "medicaid expansion",                -1,
  "food stamps",                       -1,
  "unemployment insurance",            -1,
  "unemployment benefits",             -1,
  "social security",                   -1,
  "medicare",                          -1,
  "affordable healthcare",             -1,
  "healthcare for all",                -1,
  "universal healthcare",              -1,
  "single payer",                      -1,
  "public option",                     -1,
  "affordable care act",               -1,
  "obamacare",                         -1,
  "preexisting conditions",            -1,
  "prescription drug costs",           -1,
  "public education",                  -1,
  "public schools",                    -1,
  "invest in education",               -1,
  "education funding",                 -1,
  "student debt",                      -1,
  "student loans",                     -1,
  "affordable college",                -1,
  "early childhood education",         -1,
  "climate change",                    -1,
  "climate crisis",                    -1,
  "global warming",                    -1,
  "clean energy",                      -1,
  "renewable energy",                  -1,
  "green jobs",                        -1,
  "environmental protection",          -1,
  "environmental justice",             -1,
  "carbon emissions",                  -1,
  "fossil fuels",                      -1,
  "civil rights",                      -1,
  "voting rights",                     -1,
  "voter suppression",                 -1,
  "discrimination",                    -1,
  "equal rights",                      -1,
  "social justice",                    -1,
  "racial justice",                    -1,
  "systemic racism",                   -1,
  "police reform",                     -1,
  "criminal justice reform",           -1,
  "womens rights",                     -1,
  "reproductive rights",               -1,
  "reproductive freedom",              -1,
  "reproductive health",               -1,
  "womens health",                     -1,
  "planned parenthood",                -1,
  "pro choice",                        -1,
  "right to choose",                   -1,
  "comprehensive immigration reform",  -1,
  "path to citizenship",               -1,
  "dreamers",                          -1,
  "immigration reform",                -1,
  "family separation",                 -1,
  "asylum seekers",                    -1,
  "wall street",                       -1,
  "big banks",                         -1,
  "big oil",                           -1,
  "big pharma",                        -1,
  "pharmaceutical companies",          -1,
  "corporate accountability",          -1,
  "corporate profits",                 -1,
  "special interests",                 -1,
  "public investment",                 -1,
  "infrastructure investment",         -1,
  "invest in infrastructure",          -1,
  "rebuild america",                   -1,
  "crumbling infrastructure",          -1,
  "gun violence",                      -1,
  "gun safety",                        -1,
  "common sense gun laws",             -1,
  "background checks",                 -1,
  "gun control",                       -1,
  "assault weapons",                   -1,
  "gun reform",                        -1,
  "lgbtq rights",                      -1,
  "marriage equality",                 -1,
  "same sex marriage",                 -1,
  "access to healthcare",              -1,
  "access to education",               -1,
  "affordable housing",                -1,
  "living standards",                  -1,
  "quality of life",                   -1
)

# Score speeches

count_partisan_phrases <- function(text, dictionary) {
  if (is.na(text) || text == "") {
    return(tibble(partisan_score = 0, total_phrases = 0))
  }
  phrase_counts   <- map_dbl(dictionary$phrase, ~str_count(text, fixed(.x)))
  weighted_scores <- phrase_counts * dictionary$party_score
  tibble(
    partisan_score = sum(weighted_scores),
    total_phrases  = sum(phrase_counts)
  )
}

partisan_scores <- analysis_data %>%
  rowwise() %>%
  mutate(scores = list(count_partisan_phrases(text_clean, partisan_dict))) %>%
  ungroup() %>%
  unnest(scores) %>%
  mutate(partisan_score_per1000 = -(partisan_score / word_count) * 1000)

# Validation: party discrimination pre-primary
partisan_scores %>%
  filter(post_primary == FALSE, party %in% c("Democrat", "Republican")) %>%
  group_by(party) %>%
  summarise(mean = round(mean(partisan_score_per1000, na.rm = TRUE), 3),
            n    = n()) %>%
  print()

t_result <- t.test(
  partisan_score_per1000 ~ party,
  data = filter(partisan_scores, post_primary == FALSE,
                party %in% c("Democrat", "Republican"))
)
cat("Party discrimination p-value:", round(t_result$p.value, 6), "\n")

# Aggregate to member-period level and save

custom <- partisan_scores %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    n_speeches         = n(),
    mean_custom_score  = mean(partisan_score_per1000, na.rm = TRUE),
    .groups            = "drop"
  )

write_rds(custom, "data/processed/custom.rds")