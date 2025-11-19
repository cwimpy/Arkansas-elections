# Arkansas Elections Performance Index Analysis
# Visualizations examining election administration in Arkansas

# Load required libraries
library(tidyverse)
library(scales)
library(viridis)
library(patchwork)
library(ggrepel)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        plot.caption = element_text(size = 9, color = "gray50"),
        legend.position = "bottom"))

# Define Arkansas color palette
ar_colors <- c(
  "Arkansas" = "#9D2235",  # Arkansas Red
  "National Average" = "#333333",
  "Missouri" = "#BF0A30",
  "Tennessee" = "#FF8200",
  "Louisiana" = "#461D7C",
  "Mississippi" = "#862633"
)

# Load data
indicators <- read_csv("data/epi_arkansas_indicators.csv")
rankings <- read_csv("data/epi_arkansas_rankings.csv")
scores_comparison <- read_csv("data/epi_scores_comparison.csv")
regional_comparison <- read_csv("data/epi_regional_comparison.csv")

# ============================================================================
# 1. OVERALL PERFORMANCE: Arkansas vs National Average Over Time
# ============================================================================

p1 <- scores_comparison %>%
  ggplot(aes(x = year, y = epi_score, color = state, group = state)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = ar_colors) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Arkansas EPI Score: Closing the Gap with National Average",
    subtitle = "Elections Performance Index scores, 2008-2022",
    x = "Year",
    y = "EPI Score (0-100)",
    color = "",
    caption = "Source: MIT Elections Performance Index"
  ) +
  annotate("text", x = 2020, y = 58, label = "AR improved\n10 points",
           size = 3.5, color = "#9D2235", fontface = "bold") +
  annotate("segment", x = 2008, xend = 2020, y = 48, yend = 58,
           arrow = arrow(length = unit(0.3, "cm")),
           color = "#9D2235", alpha = 0.5)

ggsave("visualizations/01_arkansas_vs_national.png", p1,
       width = 10, height = 6, dpi = 300)

# ============================================================================
# 2. REGIONAL COMPARISON: Arkansas vs Neighboring States
# ============================================================================

p2 <- regional_comparison %>%
  ggplot(aes(x = year, y = epi_score, color = state, group = state)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = ar_colors) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  geom_text_repel(
    data = regional_comparison %>% filter(year == 2022),
    aes(label = state),
    nudge_x = 0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  labs(
    title = "Regional Context: Arkansas Among Neighboring States",
    subtitle = "Only Mississippi trails Arkansas in election administration performance",
    x = "Year",
    y = "EPI Score",
    color = "",
    caption = "Source: MIT Elections Performance Index"
  ) +
  theme(legend.position = "none")

ggsave("visualizations/02_regional_comparison.png", p2,
       width = 10, height = 6, dpi = 300)

# ============================================================================
# 3. RANKING TRAJECTORY: Arkansas's National Rank Over Time
# ============================================================================

p3 <- rankings %>%
  ggplot(aes(x = year, y = arkansas_rank)) +
  geom_line(color = "#9D2235", size = 1.5) +
  geom_point(color = "#9D2235", size = 4) +
  geom_text(aes(label = paste0("#", arkansas_rank)),
            vjust = -1, fontface = "bold", color = "#9D2235") +
  scale_y_reverse(breaks = seq(30, 45, 5)) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Arkansas National Ranking: Slow but Steady Improvement",
    subtitle = "Rank among 51 states (lower is better)",
    x = "Year",
    y = "National Rank",
    caption = "Arkansas improved from 42nd (2008) to 33rd (2020), then fell to 35th (2022)"
  ) +
  annotate("rect", xmin = 2008, xmax = 2020, ymin = 30, ymax = 45,
           alpha = 0.1, fill = "green") +
  annotate("text", x = 2014, y = 31, label = "Period of Improvement",
           size = 3, color = "darkgreen", fontface = "italic")

ggsave("visualizations/03_ranking_trajectory.png", p3,
       width = 10, height = 6, dpi = 300)

# ============================================================================
# 4. INDICATOR HEATMAP: Performance Across All Indicators Over Time
# ============================================================================

# Reorder indicators by 2022 performance
indicator_order <- indicators %>%
  filter(year == 2022) %>%
  arrange(desc(score)) %>%
  pull(indicator)

p4 <- indicators %>%
  mutate(indicator = factor(indicator, levels = indicator_order)) %>%
  ggplot(aes(x = year, y = indicator, fill = score)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis(option = "rocket", direction = -1,
                     labels = percent,
                     breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Arkansas EPI Indicators Heatmap: Where Does Arkansas Excel?",
    subtitle = "Performance across 18 election administration indicators (darker = better)",
    x = "Year",
    y = "",
    fill = "Score",
    caption = "Source: MIT Elections Performance Index"
  ) +
  theme(axis.text.y = element_text(size = 9),
        panel.grid = element_blank())

ggsave("visualizations/04_indicator_heatmap.png", p4,
       width = 12, height = 8, dpi = 300)

# ============================================================================
# 5. STRENGTHS & WEAKNESSES: Current Performance (2022)
# ============================================================================

p5 <- indicators %>%
  filter(year == 2022) %>%
  mutate(
    indicator = fct_reorder(indicator, score),
    performance = case_when(
      score >= 0.75 ~ "Strong",
      score >= 0.5 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ggplot(aes(x = score, y = indicator, fill = performance)) +
  geom_col() +
  geom_text(aes(label = percent(score, accuracy = 1)),
            hjust = -0.1, size = 3) +
  scale_fill_manual(values = c("Strong" = "#2E7D32",
                                "Moderate" = "#F57C00",
                                "Weak" = "#C62828")) +
  scale_x_continuous(labels = percent, limits = c(0, 1.1),
                     breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Arkansas 2022 Indicator Performance: Strengths and Weaknesses",
    subtitle = "Where Arkansas excels and where improvement is needed",
    x = "Performance Score",
    y = "",
    fill = "Performance Level",
    caption = "Source: MIT Elections Performance Index (2022)"
  )

ggsave("visualizations/05_strengths_weaknesses_2022.png", p5,
       width = 10, height = 8, dpi = 300)

# ============================================================================
# 6. CHANGE OVER TIME: Which Indicators Improved Most?
# ============================================================================

indicator_change <- indicators %>%
  filter(year %in% c(2008, 2022)) %>%
  pivot_wider(names_from = year, values_from = score, names_prefix = "year_") %>%
  mutate(
    change = year_2022 - year_2008,
    change_type = ifelse(change > 0, "Improved", "Declined")
  ) %>%
  arrange(desc(abs(change)))

p6 <- indicator_change %>%
  mutate(indicator = fct_reorder(indicator, change)) %>%
  ggplot(aes(x = change, y = indicator, fill = change_type)) +
  geom_col() +
  geom_text(aes(label = sprintf("%+.2f", change)),
            hjust = ifelse(indicator_change$change > 0, -0.1, 1.1),
            size = 3) +
  scale_fill_manual(values = c("Improved" = "#2E7D32", "Declined" = "#C62828")) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "14-Year Change in Arkansas EPI Indicators (2008-2022)",
    subtitle = "Online registration and post-election audits show dramatic improvement",
    x = "Change in Score (2022 - 2008)",
    y = "",
    fill = "",
    caption = "Source: MIT Elections Performance Index"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

ggsave("visualizations/06_indicator_changes.png", p6,
       width = 10, height = 8, dpi = 300)

# ============================================================================
# 7. KEY INDICATORS DEEP DIVE: Critical Metrics Over Time
# ============================================================================

key_indicators <- c("Online Registration", "Registration Accuracy",
                   "Wait Times", "Turnout", "Post-Election Audits",
                   "Residual Vote Rate")

p7 <- indicators %>%
  filter(indicator %in% key_indicators) %>%
  ggplot(aes(x = year, y = score, color = indicator)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~indicator, ncol = 3, scales = "free_y") +
  scale_color_viridis_d(option = "plasma") +
  scale_x_continuous(breaks = c(2008, 2014, 2020)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Key Indicator Trends: A Closer Look at Critical Metrics",
    subtitle = "Online registration adopted in 2020; other metrics show mixed performance",
    x = "Year",
    y = "Score",
    caption = "Source: MIT Elections Performance Index"
  ) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 10))

ggsave("visualizations/07_key_indicators_trends.png", p7,
       width = 12, height = 6, dpi = 300)

# ============================================================================
# 8. COMPARATIVE SCATTER: 2008 vs 2022 Performance
# ============================================================================

comparison_data <- indicators %>%
  filter(year %in% c(2008, 2022)) %>%
  pivot_wider(names_from = year, values_from = score, names_prefix = "year_") %>%
  mutate(
    improved = year_2022 > year_2008,
    big_change = abs(year_2022 - year_2008) > 0.2
  )

p8 <- comparison_data %>%
  ggplot(aes(x = year_2008, y = year_2022)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", size = 1) +
  geom_point(aes(color = improved, size = big_change), alpha = 0.7) +
  geom_text_repel(
    data = comparison_data %>% filter(big_change),
    aes(label = indicator),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828"),
                     labels = c("Declined", "Improved")) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2),
                    guide = "none") +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "2008 vs 2022: Which Indicators Changed Most?",
    subtitle = "Points above the diagonal line represent improvement",
    x = "2008 Score",
    y = "2022 Score",
    color = "",
    caption = "Source: MIT Elections Performance Index\nLarger points indicate changes > 20 percentage points"
  ) +
  annotate("text", x = 0.2, y = 0.9, label = "Improved",
           color = "#2E7D32", fontface = "bold", size = 5) +
  annotate("text", x = 0.9, y = 0.2, label = "Declined",
           color = "#C62828", fontface = "bold", size = 5)

ggsave("visualizations/08_2008_vs_2022_scatter.png", p8,
       width = 10, height = 10, dpi = 300)

# ============================================================================
# 9. POLICY INNOVATIONS: Binary Indicators (Adoption Timeline)
# ============================================================================

# Binary indicators (0 or close to 1)
binary_indicators <- c("Online Registration", "Post-Election Audits",
                      "ERIC Membership", "Risk-Limiting Audits", "Early Voting")

p9 <- indicators %>%
  filter(indicator %in% binary_indicators) %>%
  mutate(adopted = score > 0.5) %>%
  ggplot(aes(x = year, y = indicator, fill = adopted)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_manual(values = c("FALSE" = "#FFCDD2", "TRUE" = "#2E7D32"),
                    labels = c("Not Adopted", "Adopted")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Policy Adoption Timeline: When Did Arkansas Implement Key Reforms?",
    subtitle = "Arkansas has adopted some reforms but lags on others",
    x = "Year",
    y = "",
    fill = "Status",
    caption = "Source: MIT Elections Performance Index"
  ) +
  theme(panel.grid = element_blank())

ggsave("visualizations/09_policy_adoption.png", p9,
       width = 10, height = 5, dpi = 300)

# ============================================================================
# 10. OVERALL SCORE DECOMPOSITION: What's Driving the Score?
# ============================================================================

# Calculate contribution of each indicator to total score
p10 <- indicators %>%
  filter(year == 2022) %>%
  arrange(desc(score)) %>%
  mutate(
    indicator = fct_reorder(indicator, score),
    cumulative = cumsum(score),
    score_pct = score / sum(score)
  ) %>%
  ggplot(aes(x = score, y = indicator, fill = score)) +
  geom_col() +
  scale_fill_viridis(option = "rocket", direction = -1, guide = "none") +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  labs(
    title = "2022 Indicator Contributions: What Makes Up Arkansas's Score?",
    subtitle = "All 18 indicators ranked by performance level",
    x = "Indicator Score",
    y = "",
    caption = "Source: MIT Elections Performance Index (2022)"
  ) +
  theme(axis.text.y = element_text(size = 9))

ggsave("visualizations/10_score_decomposition.png", p10,
       width = 10, height = 8, dpi = 300)

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== ARKANSAS EPI ANALYSIS SUMMARY ===\n\n")

cat("Overall Performance:\n")
cat(sprintf("  2008 Score: %.1f\n",
            scores_comparison %>% filter(state == "Arkansas", year == 2008) %>% pull(epi_score)))
cat(sprintf("  2022 Score: %.1f\n",
            scores_comparison %>% filter(state == "Arkansas", year == 2022) %>% pull(epi_score)))
cat(sprintf("  Improvement: %.1f points\n\n",
            scores_comparison %>% filter(state == "Arkansas", year == 2022) %>% pull(epi_score) -
            scores_comparison %>% filter(state == "Arkansas", year == 2008) %>% pull(epi_score)))

cat("Rankings:\n")
cat(sprintf("  2008 Rank: #%d of 51\n",
            rankings %>% filter(year == 2008) %>% pull(arkansas_rank)))
cat(sprintf("  2022 Rank: #%d of 51\n",
            rankings %>% filter(year == 2022) %>% pull(arkansas_rank)))
cat(sprintf("  Rank Improvement: %d positions\n\n",
            rankings %>% filter(year == 2008) %>% pull(arkansas_rank) -
            rankings %>% filter(year == 2022) %>% pull(arkansas_rank)))

cat("Top 5 Performing Indicators (2022):\n")
top_indicators <- indicators %>%
  filter(year == 2022) %>%
  arrange(desc(score)) %>%
  head(5)
for(i in 1:nrow(top_indicators)) {
  cat(sprintf("  %d. %s: %.1f%%\n", i,
              top_indicators$indicator[i],
              top_indicators$score[i] * 100))
}

cat("\nBottom 5 Performing Indicators (2022):\n")
bottom_indicators <- indicators %>%
  filter(year == 2022) %>%
  arrange(score) %>%
  head(5)
for(i in 1:nrow(bottom_indicators)) {
  cat(sprintf("  %d. %s: %.1f%%\n", i,
              bottom_indicators$indicator[i],
              bottom_indicators$score[i] * 100))
}

cat("\nBiggest Improvements (2008-2022):\n")
improvements <- indicator_change %>%
  arrange(desc(change)) %>%
  head(5)
for(i in 1:nrow(improvements)) {
  cat(sprintf("  %d. %s: %+.1f points\n", i,
              improvements$indicator[i],
              improvements$change[i] * 100))
}

cat("\nBiggest Declines (2008-2022):\n")
declines <- indicator_change %>%
  arrange(change) %>%
  head(5)
for(i in 1:nrow(declines)) {
  cat(sprintf("  %d. %s: %+.1f points\n", i,
              declines$indicator[i],
              declines$change[i] * 100))
}

cat("\n=== Analysis complete! ===\n")
cat("All visualizations saved to 'visualizations/' directory\n")
