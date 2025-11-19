# Quick Start: Arkansas EPI Data Exploration
# A simplified script for initial data exploration

library(tidyverse)

# Load data
cat("Loading Arkansas EPI data...\n\n")
indicators <- read_csv("data/epi_arkansas_indicators.csv", show_col_types = FALSE)
rankings <- read_csv("data/epi_arkansas_rankings.csv", show_col_types = FALSE)
scores <- read_csv("data/epi_scores_comparison.csv", show_col_types = FALSE)
regional <- read_csv("data/epi_regional_comparison.csv", show_col_types = FALSE)

# ============================================================================
# QUICK SUMMARY STATISTICS
# ============================================================================

cat("=== ARKANSAS ELECTIONS PERFORMANCE INDEX: QUICK OVERVIEW ===\n\n")

# Overall trajectory
cat("OVERALL SCORE TRAJECTORY:\n")
ar_scores <- scores %>% filter(state == "Arkansas")
cat(sprintf("  Start (2008): %.1f\n", ar_scores$epi_score[1]))
cat(sprintf("  End (2022):   %.1f\n", ar_scores$epi_score[nrow(ar_scores)]))
cat(sprintf("  Change:       %+.1f points\n\n",
            ar_scores$epi_score[nrow(ar_scores)] - ar_scores$epi_score[1]))

# National ranking
cat("NATIONAL RANKING:\n")
cat(sprintf("  Start (2008): #%d of 51\n", rankings$arkansas_rank[1]))
cat(sprintf("  End (2022):   #%d of 51\n", rankings$arkansas_rank[nrow(rankings)]))
cat(sprintf("  Improvement:  %+d positions\n\n",
            rankings$arkansas_rank[1] - rankings$arkansas_rank[nrow(rankings)]))

# Gap from national average
cat("GAP FROM NATIONAL AVERAGE:\n")
gap_2008 <- scores %>% filter(year == 2008) %>%
  pivot_wider(names_from = state, values_from = epi_score) %>%
  mutate(gap = `National Average` - Arkansas) %>% pull(gap)
gap_2022 <- scores %>% filter(year == 2022) %>%
  pivot_wider(names_from = state, values_from = epi_score) %>%
  mutate(gap = `National Average` - Arkansas) %>% pull(gap)
cat(sprintf("  2008: %.1f points behind\n", gap_2008))
cat(sprintf("  2022: %.1f points behind\n", gap_2022))
cat(sprintf("  Change: Gap %s by %.1f points\n\n",
            ifelse(gap_2022 > gap_2008, "widened", "narrowed"),
            abs(gap_2022 - gap_2008)))

# ============================================================================
# TOP PERFORMERS (2022)
# ============================================================================

cat("TOP 5 PERFORMING INDICATORS (2022):\n")
top_5 <- indicators %>%
  filter(year == 2022) %>%
  arrange(desc(score)) %>%
  head(5)

for (i in 1:5) {
  cat(sprintf("  %d. %-30s %.1f%%\n",
              i, top_5$indicator[i], top_5$score[i] * 100))
}

cat("\nBOTTOM 5 PERFORMING INDICATORS (2022):\n")
bottom_5 <- indicators %>%
  filter(year == 2022) %>%
  arrange(score) %>%
  head(5)

for (i in 1:5) {
  cat(sprintf("  %d. %-30s %.1f%%\n",
              i, bottom_5$indicator[i], bottom_5$score[i] * 100))
}

# ============================================================================
# BIGGEST CHANGES
# ============================================================================

cat("\nBIGGEST IMPROVEMENTS (2008-2022):\n")
changes <- indicators %>%
  filter(year %in% c(2008, 2022)) %>%
  pivot_wider(names_from = year, values_from = score) %>%
  mutate(change = `2022` - `2008`) %>%
  arrange(desc(change))

for (i in 1:5) {
  cat(sprintf("  %d. %-30s %+.1f points\n",
              i, changes$indicator[i], changes$change[i] * 100))
}

cat("\nBIGGEST DECLINES (2008-2022):\n")
declines <- changes %>% arrange(change) %>% head(5)
for (i in 1:5) {
  cat(sprintf("  %d. %-30s %+.1f points\n",
              i, declines$indicator[i], declines$change[i] * 100))
}

# ============================================================================
# REGIONAL CONTEXT
# ============================================================================

cat("\nREGIONAL COMPARISON (2022 EPI Scores):\n")
regional_2022 <- regional %>%
  filter(year == 2022) %>%
  arrange(desc(epi_score))

for (i in 1:nrow(regional_2022)) {
  marker <- ifelse(regional_2022$state[i] == "Arkansas", " <<<", "")
  cat(sprintf("  %-15s %.1f%s\n",
              regional_2022$state[i],
              regional_2022$epi_score[i],
              marker))
}

# ============================================================================
# SIMPLE VISUALIZATION
# ============================================================================

cat("\nGenerating quick visualization...\n")

# Create a simple combo plot
library(patchwork)

p1 <- scores %>%
  ggplot(aes(x = year, y = epi_score, color = state, group = state)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Arkansas" = "#9D2235",
                                 "National Average" = "#333333")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(title = "Arkansas vs National Average",
       x = NULL, y = "EPI Score", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- rankings %>%
  ggplot(aes(x = year, y = arkansas_rank)) +
  geom_line(color = "#9D2235", size = 1.2) +
  geom_point(color = "#9D2235", size = 3) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(title = "Arkansas National Ranking",
       x = "Year", y = "Rank (of 51)") +
  theme_minimal()

combined <- p1 / p2

ggsave("visualizations/00_quick_overview.png", combined,
       width = 10, height = 8, dpi = 300)

cat("\nQuick overview plot saved to: visualizations/00_quick_overview.png\n")

# ============================================================================
# INDICATOR TABLE
# ============================================================================

cat("\n=== FULL INDICATOR TABLE (2022) ===\n\n")
indicators %>%
  filter(year == 2022) %>%
  arrange(desc(score)) %>%
  mutate(
    Score = percent(score, accuracy = 0.1),
    Performance = case_when(
      score >= 0.75 ~ "Strong",
      score >= 0.50 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  select(Indicator = indicator, Score, Performance) %>%
  print(n = Infinity)

cat("\n=== For full analysis, run: source('arkansas_epi_analysis.R') ===\n")
cat("=== For advanced stats, run: source('advanced_analysis.R') ===\n\n")
