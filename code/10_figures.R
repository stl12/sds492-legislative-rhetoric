# 10_figures.R
# Generates all thesis figures.
# Figure 1 coefficients standardized by pre-primary SD for comparability.
# Event study figure generated separately by 09_event_study.R.
#
# Output: figure1_speech_content.png, figure3_era_split.png,
#         figure_volume_distribution.png, figure_sample_distribution.png,
#         figure_timeline.png, figure_fw_validation.png, figure_map.png, figure_focus_era_split

library(tidyverse)
library(fixest)
library(patchwork)
library(maps)

thesis_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 10, hjust = 0,
                                      margin = margin(b = 8)),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "gray20", linewidth = 0.4),
    axis.line.y        = element_blank(),
    axis.ticks.x       = element_line(color = "gray20", linewidth = 0.3),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 9.5, color = "gray15",
                                      margin = margin(r = 4)),
    axis.text.x        = element_text(size = 9, color = "gray15"),
    axis.title.x       = element_text(size = 9.5, color = "gray15",
                                      margin = margin(t = 7)),
    legend.position    = "none",
    plot.margin        = margin(10, 20, 5, 5)
  )

# Load data and merge covariates

covariates <- read_rds("data/processed/covariates.rds")

merge_cov <- function(df) {
  df %>%
    left_join(covariates %>% select(bioguide, Year, nominate_dim1),
              by = c("bioguide", "Year")) %>%
    filter(!is.na(nominate_dim1)) %>%
    mutate(party_factor = factor(party))
}

fw_raw        <- read_rds("data/processed/fw.rds")
custom_raw    <- read_rds("data/processed/custom.rds")
certainty_raw <- read_rds("data/processed/certainty.rds")
focus_raw     <- read_rds("data/processed/focus.rds")

fw_reg        <- merge_cov(fw_raw)
certainty_reg <- merge_cov(certainty_raw)
focus_reg     <- merge_cov(focus_raw)

# Pre-primary SDs for standardization
sd_fw        <- sd(fw_raw        %>% filter(post_primary == FALSE) %>% pull(mean_fw_score),        na.rm = TRUE)
sd_certainty <- sd(certainty_raw %>% filter(post_primary == FALSE) %>% pull(mean_certainty_score), na.rm = TRUE)
sd_focus     <- sd(focus_raw     %>% filter(post_primary == FALSE) %>% pull(mean_focus_score),     na.rm = TRUE)

# FIGURE 1: Speech Content Outcomes (standardized coefficient plot)

mod_fw <- feols(mean_fw_score ~ post_primary + nominate_dim1 |
                  bioguide + party_factor^Year, data = fw_reg, vcov = "HC1")
mod_certainty <- feols(mean_certainty_score ~ post_primary + nominate_dim1 |
                         bioguide + party_factor^Year, data = certainty_reg, vcov = "HC1")
mod_focus <- feols(mean_focus_score ~ post_primary + nominate_dim1 |
                     bioguide + party_factor^Year, data = focus_reg, vcov = "HC1")

extract_coef <- function(model, label, sd = 1) {
  pp       <- grep("^post_primary", names(coef(model)), value = TRUE)[1]
  coef_val <- coef(model)[pp] / sd
  se_val   <- se(model)[pp]   / sd
  p_val    <- pvalue(model)[pp]
  tibble(
    outcome     = label,
    coefficient = coef_val,
    se          = se_val,
    ci_low      = coef_val - 1.96  * se_val,
    ci_high     = coef_val + 1.96  * se_val,
    ci_low90    = coef_val - 1.645 * se_val,
    ci_high90   = coef_val + 1.645 * se_val,
    p_value     = p_val,
    significant = p_val < 0.1
  )
}

panel_a_data <- bind_rows(
  extract_coef(mod_fw,        "Partisan Tenor\n(Fightin' Words)", sd = sd_fw),
  extract_coef(mod_certainty, "Certainty/Hedging",                sd = sd_certainty),
  extract_coef(mod_focus,     "Constituency Focus",               sd = sd_focus)
) %>%
  mutate(outcome = factor(outcome, levels = rev(c(
    "Partisan Tenor\n(Fightin' Words)", "Certainty/Hedging", "Constituency Focus"
  ))))

x_range_a <- range(c(panel_a_data$ci_low, panel_a_data$ci_high), na.rm = TRUE)
x_lim_a   <- c(x_range_a[1] - diff(x_range_a) * 0.15,
               x_range_a[2] + diff(x_range_a) * 0.15)

panel_a <- ggplot(panel_a_data, aes(x = coefficient, y = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_low,   xmax = ci_high),
                 height = 0.12, linewidth = 0.45, color = "gray30") +
  geom_errorbarh(aes(xmin = ci_low90, xmax = ci_high90),
                 height = 0, linewidth = 1.1, color = "gray20") +
  geom_point(aes(shape = significant), size = 2.8, color = "black") +
  scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
  coord_cartesian(xlim = x_lim_a) +
  labs(x = "DiD Coefficient (standardized by pre-primary SD)", y = NULL) +
  thesis_theme

ggsave("figures/figure1_speech_content.png", panel_a,
       width = 6, height = 4, dpi = 300, bg = "white")

# FIGURE 3: Era Split (FW only, raw coefficients)

mod_fw_pre  <- feols(mean_fw_score ~ post_primary + nominate_dim1 |
                       bioguide + party_factor^Year,
                     data = filter(fw_reg, Year < 1995), vcov = "HC1")
mod_fw_post <- feols(mean_fw_score ~ post_primary + nominate_dim1 |
                       bioguide + party_factor^Year,
                     data = filter(fw_reg, Year >= 1995), vcov = "HC1")

panel_b_data <- bind_rows(
  extract_coef(mod_fw_pre,  "Pre-1994"),
  extract_coef(mod_fw_post, "Post-1994")
) %>%
  mutate(outcome = factor(outcome, levels = rev(c("Pre-1994", "Post-1994"))))

x_range_b <- range(c(panel_b_data$ci_low, panel_b_data$ci_high), na.rm = TRUE)
x_lim_b   <- c(x_range_b[1] - diff(x_range_b) * 0.15,
               x_range_b[2] + diff(x_range_b) * 0.15)

panel_b <- ggplot(panel_b_data, aes(x = coefficient, y = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = pmax(ci_low,   x_lim_b[1]),
                     xmax = pmin(ci_high,   x_lim_b[2])),
                 height = 0.12, linewidth = 0.45, color = "gray30") +
  geom_errorbarh(aes(xmin = pmax(ci_low90, x_lim_b[1]),
                     xmax = pmin(ci_high90, x_lim_b[2])),
                 height = 0, linewidth = 1.1, color = "gray20") +
  geom_point(aes(shape = significant), size = 2.8, color = "black") +
  scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
  coord_cartesian(xlim = x_lim_b) +
  labs(x = "DiD Coefficient (Post-Primary)", y = NULL) +
  thesis_theme

ggsave("figures/figure3_era_split.png", panel_b,
       width = 6, height = 3.5, dpi = 300, bg = "white")

# FIGURE 2: Speech Volume (pre vs post dot plot)

analysis_data <- read_rds("data/processed/final_analysis_data_CLEAN.rds")

member_volume <- analysis_data %>%
  group_by(bioguide, Name, Year, party, post_primary) %>%
  summarise(
    n_speeches  = n(),
    total_words = sum(word_count, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    period    = if_else(post_primary, "Post-Primary", "Pre-Primary"),
    log_words = log(total_words + 1)
  )

summary_stats <- member_volume %>%
  group_by(period) %>%
  summarise(
    mean   = mean(log_words),
    se     = sd(log_words) / sqrt(n()),
    ci_lo  = mean - 1.96 * se,
    ci_hi  = mean + 1.96 * se,
    n      = n(),
    mean_n = round(mean(n_speeches), 1),
    .groups = "drop"
  ) %>%
  mutate(period = factor(period, levels = c("Pre-Primary", "Post-Primary")))

p_volume <- ggplot(summary_stats, aes(x = period, y = mean, color = period)) +
  geom_line(aes(group = 1), color = "gray50", linewidth = 0.8, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.06, linewidth = 0.9) +
  geom_point(size = 4.5) +
  geom_text(aes(y = ci_lo - 0.08,
                label = paste0("n = ", n, "\n(mean ", mean_n, " speeches)")),
            size = 3, color = "gray40", vjust = 1) +
  scale_color_manual(values = c("Pre-Primary"  = "#2E4057",
                                "Post-Primary" = "#E63946")) +
  scale_y_continuous(name = "Mean Log Total Words", limits = c(7.4, 10.4)) +
  annotate("text", x = 1.5, y = 9.6,
           label = "\u2193 47% reduction\n(\u03b2 = \u22120.638, p < 0.001)",
           size = 3.5, color = "gray30", fontface = "italic") +
  labs(title = NULL, x = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 11, face = "bold"))

ggsave("figures/figure_volume_distribution.png", p_volume,
       width = 6, height = 3.5, dpi = 300, bg = "white")

# FIGURE 4: Sample Distribution by Congress

losers <- readxl::read_excel("data/raw/primary_losers.xlsx") %>%
  rename_with(tolower) %>%
  mutate(congress = 83 + floor((year - 1953) / 2))

congress_counts <- losers %>%
  count(congress, year) %>%
  mutate(era = if_else(year < 1995,
                       "Pre-1994 (GS corpus)",
                       "Post-1994 (ConSpeak corpus)"))

p_sample <- ggplot(congress_counts,
                   aes(x = factor(congress), y = n, fill = era)) +
  geom_col() +
  scale_fill_manual(values = c("Pre-1994 (GS corpus)"        = "#6B7280",
                               "Post-1994 (ConSpeak corpus)"  = "#2E4057")) +
  scale_x_discrete(
    breaks = as.character(seq(83, 118, by = 5)),
    labels = function(x) {
      yr <- 1954 + 2 * (as.integer(x) - 83)
      paste0(x, "\n(", yr, ")")
    }
  ) +
  labs(x = "Congress (Election Year)",
       y = "Number of Primary Losers", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position    = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 7.5))

ggsave("figures/figure_sample_distribution.png", p_sample,
       width = 8, height = 4, dpi = 300, bg = "white")

# FIGURE 5: Research Design Timeline

p_timeline <- ggplot() +
  annotate("rect", xmin = 0, xmax = 0.45, ymin = -0.08, ymax = 0.08,
           fill = "#2E4057", alpha = 0.12) +
  annotate("rect", xmin = 0.45, xmax = 1, ymin = -0.08, ymax = 0.08,
           fill = "#E63946", alpha = 0.12) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0),
               linewidth = 1.2, color = "gray30") +
  geom_segment(aes(x = 0, xend = 0.45, y = 0, yend = 0),
               linewidth = 3, color = "#2E4057", alpha = 0.8) +
  geom_segment(aes(x = 0.45, xend = 1, y = 0, yend = 0),
               linewidth = 3, color = "#E63946", alpha = 0.8) +
  geom_point(aes(x = c(0, 1), y = c(0, 0)), size = 5, color = "gray20") +
  geom_point(aes(x = 0.45, y = 0), size = 7, color = "#E63946", shape = 18) +
  annotate("text", x = 0,    y = -0.15, label = "Congress\nBegins",
           size = 3.5, color = "gray20", lineheight = 0.9) +
  annotate("text", x = 0.45, y = -0.15, label = "Primary\nLoss",
           size = 3.5, color = "gray20", lineheight = 0.9) +
  annotate("text", x = 1,    y = -0.15, label = "Congress\nEnds",
           size = 3.5, color = "gray20", lineheight = 0.9) +
  annotate("text", x = 0.225, y = 0.18, label = "Pre-Primary Period",
           size = 4, fontface = "bold", color = "#2E4057") +
  annotate("text", x = 0.225, y = 0.13, label = "rhetoric measured here",
           size = 3.2, color = "#2E4057", fontface = "italic") +
  annotate("text", x = 0.725, y = 0.18, label = "Post-Primary Period",
           size = 4, fontface = "bold", color = "#E63946") +
  annotate("text", x = 0.725, y = 0.13, label = "rhetoric measured here",
           size = 3.2, color = "#E63946", fontface = "italic") +
  annotate("segment", x = 0.225, xend = 0.725, y = 0.26, yend = 0.26,
           arrow = arrow(ends = "both", length = unit(0.2, "cm")),
           color = "gray40", linewidth = 0.6) +
  annotate("text", x = 0.475, y = 0.30,
           label = "DiD: within-member comparison",
           size = 3.5, color = "gray30", fontface = "italic") +
  annotate("segment", x = 0.28, xend = 0.45, y = -0.28, yend = -0.28,
           arrow = arrow(ends = "both", length = unit(0.15, "cm")),
           color = "gray50", linewidth = 0.5, linetype = "dashed") +
  annotate("segment", x = 0.45, xend = 0.62, y = -0.28, yend = -0.28,
           arrow = arrow(ends = "both", length = unit(0.15, "cm")),
           color = "gray50", linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 0.475, y = -0.34,
           label = "Balanced window (volume measure only, following Kim 2025)",
           size = 3, color = "gray50", fontface = "italic") +
  annotate("point", x = 0.28, y = -0.28, size = 2, color = "gray50", shape = 4) +
  xlim(-0.05, 1.05) + ylim(-0.45, 0.40) +
  theme_void(base_size = 11) +
  theme(plot.margin = margin(15, 20, 10, 20))

ggsave("figures/figure_timeline.png", p_timeline,
       width = 9, height = 4, dpi = 300, bg = "white")

# FIGURE A5: FW Dictionary Validation

fw_dict <- read_rds("data/processed/fw_dictionary.rds")

clean_phrase <- function(x) {
  x %>% str_replace_all("_", " ") %>% str_to_title()
}

exclude <- c("speaker ask", "black caucus", "previous question",
             "rule committe", "yield myself", "further ask",
             "clerk read", "congression black")

dem_top <- fw_dict %>%
  filter(fw_score > 0, !bigram %in% exclude) %>%
  slice_max(fw_score, n = 12) %>%
  mutate(phrase = clean_phrase(bigram), party = "Democratic", score = fw_score)

rep_top <- fw_dict %>%
  filter(fw_score < 0, !bigram %in% exclude) %>%
  slice_min(fw_score, n = 12) %>%
  mutate(phrase = clean_phrase(bigram), party = "Republican", score = abs(fw_score))

plot_data <- bind_rows(dem_top, rep_top) %>%
  mutate(phrase = factor(phrase,
                         levels = rev(c(dem_top$phrase, rep_top$phrase))))

p_fw <- ggplot(plot_data, aes(x = score, y = phrase, fill = party)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", score)),
            hjust = -0.15, size = 3, color = "gray30") +
  facet_wrap(~party, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Democratic" = "#1D4ED8",
                               "Republican" = "#B91C1C")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title   = "Top Partisan Phrases by Party — Fightin' Words Dictionary",
    x       = "FW Score (absolute value)", y = NULL,
    caption = str_wrap(paste0(
      "Notes: Top 12 phrases by absolute Fightin' Words score for each party. ",
      "Scores reflect the standardized log odds ratio with Dirichlet prior ",
      "(Monroe et al. 2008). Phrases are stemmed bigrams; higher scores indicate ",
      "stronger partisan distinctiveness. Procedural phrases excluded. ",
      "Full dictionary contains 17,691 phrases."), width = 120)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    plot.title         = element_text(face = "bold", size = 11),
    plot.caption       = element_text(size = 7.5, hjust = 0, color = "gray40",
                                      margin = margin(t = 10)),
    axis.text.y        = element_text(size = 9)
  )

ggsave("figures/figure_fw_validation.png", p_fw,
       width = 10, height = 7, dpi = 300, bg = "white")

# FIGURE A4: Geographic Map

state_counts <- losers %>%
  count(state, name = "n_losers") %>%
  mutate(state_name = state.name[match(state, state.abb)]) %>%
  filter(!is.na(state_name))

us_map <- map_data("state") %>%
  mutate(state_name = str_to_title(region)) %>%
  left_join(state_counts, by = "state_name") %>%
  mutate(n_losers = replace_na(n_losers, 0))

p_map <- ggplot(us_map, aes(x = long, y = lat, group = group,
                            fill = n_losers)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#D9E2EC", high = "#2E4057",
                      name = "Primary\nLosers",
                      breaks = c(0, 5, 10, 15, 20), limits = c(0, NA)) +
  coord_fixed(1.3) +
  theme_void(base_size = 11) +
  theme(legend.position = "right",
        plot.margin     = margin(10, 10, 10, 10))

ggsave("figures/figure_map.png", p_map,
       width = 8, height = 5, dpi = 300, bg = "white")

# FIGURE 5: Constituency Focus Era Split (poster heterogeneity figure)

focus_reg_era <- focus_reg

mod_focus_pre  <- feols(mean_focus_score ~ post_primary + nominate_dim1 |
                          bioguide + party_factor^Year,
                        data = filter(focus_reg_era, Year < 1995), vcov = "HC1")
mod_focus_post <- feols(mean_focus_score ~ post_primary + nominate_dim1 |
                          bioguide + party_factor^Year,
                        data = filter(focus_reg_era, Year >= 1995), vcov = "HC1")

panel_focus_data <- bind_rows(
  extract_coef(mod_focus_pre,  "Pre-1994"),
  extract_coef(mod_focus_post, "Post-1994")
) %>%
  mutate(outcome = factor(outcome, levels = rev(c("Pre-1994", "Post-1994"))))

x_range_focus <- range(c(panel_focus_data$ci_low, panel_focus_data$ci_high), na.rm = TRUE)
x_lim_focus   <- c(x_range_focus[1] - diff(x_range_focus) * 0.15,
                   x_range_focus[2] + diff(x_range_focus) * 0.15)

panel_focus <- ggplot(panel_focus_data, aes(x = coefficient, y = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = pmax(ci_low,   x_lim_focus[1]),
                     xmax = pmin(ci_high,   x_lim_focus[2])),
                 height = 0.12, linewidth = 0.45, color = "gray30") +
  geom_errorbarh(aes(xmin = pmax(ci_low90, x_lim_focus[1]),
                     xmax = pmin(ci_high90, x_lim_focus[2])),
                 height = 0, linewidth = 1.1, color = "gray20") +
  geom_point(aes(shape = significant), size = 2.8, color = "black") +
  scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
  coord_cartesian(xlim = x_lim_focus) +
  labs(x = "DiD Coefficient (Post-Primary)", y = NULL) +
  thesis_theme

ggsave("figures/figure_focus_era_split.png", panel_focus,
       width = 6, height = 3.5, dpi = 300, bg = "white")