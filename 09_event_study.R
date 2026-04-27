# 09_event_study.R
# Event study: FW partisan tenor around primary loss.
# Tests parallel trends assumption.
# Reference period: one quarter before primary loss.
# Output: event_study_fw.png

library(tidyverse)
library(fixest)

# Build quarterly speech-level dataset

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds")
fw_dict       <- read_rds("data/processed/fw_dictionary.rds")

# Load FW speech-level scores
# Re-score using analysis_data with date information

clean_text <- function(text) {
  if (is.na(text) || text == "") return("")
  text %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ") %>%
    tolower() %>%
    str_replace_all("-|'", " ") %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}

stem_words <- function(text) {
  if (is.na(text) || text == "") return("")
  words   <- str_split(text, "\\s+")[[1]]
  words   <- words[str_length(words) > 1]
  stemmed <- wordStem(words, language = "english")
  paste(stemmed, collapse = " ")
}

score_fw <- function(text_stemmed, dict) {
  if (is.na(text_stemmed) || text_stemmed == "") return(0)
  bigrams       <- tibble(text = text_stemmed) %>%
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    pull(bigram)
  if (length(bigrams) == 0) return(0)
  bigram_counts <- table(bigrams)
  matched <- dict %>%
    filter(bigram %in% names(bigram_counts)) %>%
    mutate(count = as.integer(bigram_counts[bigram]))
  if (nrow(matched) == 0) return(0)
  sum(matched$fw_score * matched$count) / sum(matched$count)
}

fw_speeches <- analysis_data %>%
  mutate(
    text_clean   = map_chr(text, clean_text),
    text_stemmed = map_chr(text_clean, stem_words),
    fw_score     = map_dbl(text_stemmed, ~score_fw(.x, fw_dict)),
    days_to_primary   = as.numeric(difftime(date, Primary_Date, units = "days")),
    quarter_to_primary = floor((days_to_primary / 30.44) / 3)
  ) %>%
  filter(quarter_to_primary >= -4, quarter_to_primary <= 2)

# Aggregate to member-quarter level

covariates <- read_rds("data/processed/covariates.rds") %>%
  select(bioguide, Year, nominate_dim1)

member_quarter <- fw_speeches %>%
  group_by(bioguide, Year, party, quarter_to_primary) %>%
  summarise(
    mean_fw    = mean(fw_score, na.rm = TRUE),
    n_speeches = n(),
    .groups    = "drop"
  ) %>%
  left_join(covariates, by = c("bioguide", "Year")) %>%
  filter(!is.na(nominate_dim1)) %>%
  mutate(
    party_factor   = factor(party),
    quarter_factor = relevel(factor(quarter_to_primary), ref = "-1")
  )

# Event study regression

event_mod <- feols(
  mean_fw ~ quarter_factor + nominate_dim1 | bioguide + party_factor^Year,
  data   = member_quarter,
  vcov   = "HC1"
)

# Extract and plot

coef_table <- coeftable(event_mod) %>%
  as_tibble(rownames = "term") %>%
  filter(str_starts(term, "quarter_factor")) %>%
  mutate(
    quarter = as.numeric(str_remove(term, "quarter_factor")),
    ci_low  = Estimate - 1.96 * `Std. Error`,
    ci_high = Estimate + 1.96 * `Std. Error`
  ) %>%
  bind_rows(tibble(quarter = -1, Estimate = 0, `Std. Error` = 0,
                   ci_low = 0, ci_high = 0)) %>%
  arrange(quarter) %>%
  mutate(months = quarter * 3)

p_event <- ggplot(coef_table, aes(x = months, y = Estimate)) +
  annotate("rect", xmin = 0, xmax = max(coef_table$months) + 1,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.08) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "black", linewidth = 0.8, alpha = 0.7) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color  = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2.5) +
  labs(
    title    = "Event Study: Partisan Tenor Around Primary Loss",
    subtitle = "Coefficients relative to one quarter before primary loss",
    x        = "Months Relative to Primary Loss",
    y        = "Effect on Partisan Tenor (FW)",
    caption  = paste0("Shaded region = post-primary period. Bands show 95% CIs. ",
                      "Member and party\u00d7year FEs included. HC1 standard errors. ",
                      "Reference period: one quarter before primary loss.")
  ) +
  scale_x_continuous(breaks = coef_table$months,
                     labels = coef_table$months) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "gray40"),
    plot.caption     = element_text(size = 8, hjust = 0, color = "gray40"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/event_study_fw.png", p_event,
       width = 8, height = 4.5, dpi = 300, bg = "white")