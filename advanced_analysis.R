# Advanced Statistical Analysis of Arkansas EPI Data
# Includes trend analysis, regression, clustering, and forecasting

library(tidyverse)
library(broom)
library(scales)
library(viridis)
library(ggrepel)

# Load data
indicators <- read_csv("data/epi_arkansas_indicators.csv")
scores_comparison <- read_csv("data/epi_scores_comparison.csv")
all_states <- read_csv("data/epi indicators-all years.csv")

# ============================================================================
# 1. TREND ANALYSIS: Linear regression for each indicator
# ============================================================================

trend_analysis <- indicators %>%
  group_by(indicator) %>%
  do({
    model <- lm(score ~ year, data = .)
    tibble(
      slope = coef(model)[2],
      p_value = summary(model)$coefficients[2, 4],
      r_squared = summary(model)$r.squared,
      significant = summary(model)$coefficients[2, 4] < 0.05
    )
  }) %>%
  ungroup() %>%
  arrange(desc(abs(slope)))

cat("\n=== INDICATOR TRENDS (Linear Regression) ===\n\n")
cat("Indicators with significant trends (p < 0.05):\n\n")

trend_analysis %>%
  filter(significant) %>%
  mutate(
    trend = ifelse(slope > 0, "Increasing", "Decreasing"),
    annual_change = slope * 100
  ) %>%
  select(indicator, trend, annual_change, r_squared, p_value) %>%
  arrange(desc(annual_change)) %>%
  print(n = Infinity)

# Visualization
p1 <- trend_analysis %>%
  mutate(
    indicator = fct_reorder(indicator, slope),
    direction = ifelse(slope > 0, "Improving", "Declining")
  ) %>%
  ggplot(aes(x = slope * 100, y = indicator, fill = significant)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "gray70"),
                    labels = c("Not Significant", "Significant (p<0.05)")) +
  labs(
    title = "Annual Rate of Change by Indicator",
    subtitle = "Based on linear regression (2008-2022)",
    x = "Annual Change (percentage points per year)",
    y = "",
    fill = "Trend Status"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave("visualizations/11_trend_analysis.png", p1, width = 10, height = 8, dpi = 300)

# ============================================================================
# 2. ACCELERATION/DECELERATION: Is the gap closing or widening?
# ============================================================================

gap_analysis <- scores_comparison %>%
  pivot_wider(names_from = state, values_from = epi_score) %>%
  mutate(gap = `National Average` - Arkansas) %>%
  select(year, Arkansas, `National Average`, gap)

# Fit quadratic model to see if gap is accelerating
gap_model <- lm(gap ~ year + I(year^2), data = gap_analysis)

cat("\n=== GAP ANALYSIS ===\n\n")
cat(sprintf("Current gap (2022): %.1f points\n",
            gap_analysis %>% filter(year == 2022) %>% pull(gap)))
cat(sprintf("Historical gap (2008): %.1f points\n",
            gap_analysis %>% filter(year == 2008) %>% pull(gap)))
cat(sprintf("Gap change: %+.1f points\n\n",
            gap_analysis %>% filter(year == 2022) %>% pull(gap) -
            gap_analysis %>% filter(year == 2008) %>% pull(gap)))

# Visualization
p2 <- gap_analysis %>%
  ggplot(aes(x = year, y = gap)) +
  geom_line(size = 1.2, color = "#C62828") +
  geom_point(size = 3, color = "#C62828") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = TRUE, alpha = 0.2, color = "gray30") +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Arkansas-National Gap: Widening Over Time",
    subtitle = "Difference between National Average and Arkansas EPI scores",
    x = "Year",
    y = "Gap (National Avg - Arkansas)",
    caption = "Trend line shows quadratic fit"
  ) +
  theme_minimal()

ggsave("visualizations/12_gap_analysis.png", p2, width = 10, height = 6, dpi = 300)

# ============================================================================
# 3. INDICATOR CORRELATION: Which indicators move together?
# ============================================================================

# Reshape for correlation
indicator_wide <- indicators %>%
  pivot_wider(names_from = indicator, values_from = score)

# Calculate correlations
cor_matrix <- indicator_wide %>%
  select(-year, -state) %>%
  cor(use = "complete.obs")

# Find strongest correlations
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("indicator1") %>%
  pivot_longer(-indicator1, names_to = "indicator2", values_to = "correlation") %>%
  filter(indicator1 < indicator2) %>%  # Remove duplicates
  arrange(desc(abs(correlation)))

cat("\n=== STRONGEST CORRELATIONS ===\n\n")
cat("Top 10 positive correlations:\n")
cor_long %>%
  filter(correlation > 0) %>%
  head(10) %>%
  print()

cat("\nTop 10 negative correlations:\n")
cor_long %>%
  filter(correlation < 0) %>%
  head(10) %>%
  print()

# Heatmap
library(reshape2)
p3 <- cor_matrix %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#C62828", mid = "white", high = "#2E7D32",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title = element_blank()) +
  labs(
    title = "Indicator Correlation Matrix",
    subtitle = "How do Arkansas's election indicators relate to each other?",
    fill = "Correlation"
  )

ggsave("visualizations/13_correlation_matrix.png", p3, width = 12, height = 11, dpi = 300)

# ============================================================================
# 4. VOLATILITY ANALYSIS: Which indicators are most stable?
# ============================================================================

volatility <- indicators %>%
  group_by(indicator) %>%
  summarize(
    sd = sd(score, na.rm = TRUE),
    cv = sd(score, na.rm = TRUE) / mean(score, na.rm = TRUE),
    range = max(score, na.rm = TRUE) - min(score, na.rm = TRUE),
    mean_score = mean(score, na.rm = TRUE)
  ) %>%
  arrange(desc(sd))

cat("\n=== VOLATILITY ANALYSIS ===\n\n")
cat("Most volatile indicators (highest standard deviation):\n")
volatility %>%
  head(5) %>%
  print()

cat("\nMost stable indicators (lowest standard deviation):\n")
volatility %>%
  arrange(sd) %>%
  head(5) %>%
  print()

# Visualization
p4 <- volatility %>%
  mutate(indicator = fct_reorder(indicator, sd)) %>%
  ggplot(aes(x = mean_score, y = sd, label = indicator)) +
  geom_point(size = 3, alpha = 0.6, color = "#1976D2") +
  geom_text_repel(size = 3, max.overlaps = 20) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Indicator Volatility: Stability vs. Performance",
    subtitle = "Standard deviation over time (2008-2022)",
    x = "Mean Score",
    y = "Standard Deviation",
    caption = "Upper right = high performing but volatile; lower right = high performing and stable"
  ) +
  theme_minimal()

ggsave("visualizations/14_volatility_analysis.png", p4, width = 10, height = 8, dpi = 300)

# ============================================================================
# 5. COMPARATIVE RANKINGS: Where does Arkansas rank on each indicator?
# ============================================================================

# This requires the all_states data
# For 2022, calculate Arkansas's rank on each indicator

state_rankings_2022 <- all_states %>%
  filter(year == 2022) %>%
  select(state_abbv, online_reg, wait, residual, vep_turnout,
         post_election_audit, pct_reg_of_vep_vrs,
         nonvoter_reg_onyear_pct, eric_membership, risk_limiting_audits) %>%
  pivot_longer(-state_abbv, names_to = "indicator", values_to = "score") %>%
  group_by(indicator) %>%
  mutate(
    rank = rank(-score, ties.method = "min"),
    is_arkansas = state_abbv == "AR"
  ) %>%
  ungroup()

arkansas_ranks_2022 <- state_rankings_2022 %>%
  filter(is_arkansas) %>%
  arrange(rank)

cat("\n=== ARKANSAS RANKINGS BY INDICATOR (2022) ===\n\n")
print(arkansas_ranks_2022 %>% select(indicator, rank, score), n = Infinity)

# ============================================================================
# 6. FORECASTING: Simple projection for 2024
# ============================================================================

# Fit linear models to project 2024
forecasts <- indicators %>%
  group_by(indicator) %>%
  do({
    model <- lm(score ~ year, data = .)
    tibble(
      forecast_2024 = predict(model, newdata = data.frame(year = 2024)),
      current_2022 = filter(., year == 2022)$score
    )
  }) %>%
  ungroup() %>%
  mutate(
    forecast_2024 = pmax(0, pmin(1, forecast_2024)),  # Bound between 0 and 1
    projected_change = forecast_2024 - current_2022
  )

cat("\n=== 2024 PROJECTIONS (Based on Linear Trends) ===\n\n")
cat("Indicators projected to improve most:\n")
forecasts %>%
  arrange(desc(projected_change)) %>%
  head(5) %>%
  mutate(across(where(is.numeric), ~round(. * 100, 1))) %>%
  print()

cat("\nIndicators projected to decline:\n")
forecasts %>%
  filter(projected_change < 0) %>%
  arrange(projected_change) %>%
  mutate(across(where(is.numeric), ~round(. * 100, 1))) %>%
  print()

# Overall score projection
overall_forecast <- scores_comparison %>%
  filter(state == "Arkansas")

overall_model <- lm(epi_score ~ year, data = overall_forecast)
forecast_2024 <- predict(overall_model, newdata = data.frame(year = 2024))

cat(sprintf("\nProjected overall EPI score for 2024: %.1f\n", forecast_2024))
cat(sprintf("(Current 2022 score: %.1f)\n\n",
            overall_forecast %>% filter(year == 2022) %>% pull(epi_score)))

# ============================================================================
# 7. CHANGE POINT DETECTION: When did major changes occur?
# ============================================================================

# Look for years with large score changes
change_points <- scores_comparison %>%
  filter(state == "Arkansas") %>%
  arrange(year) %>%
  mutate(
    change = epi_score - lag(epi_score),
    abs_change = abs(change)
  ) %>%
  filter(!is.na(change))

cat("=== MAJOR CHANGE POINTS ===\n\n")
cat("Years with largest score changes:\n")
change_points %>%
  arrange(desc(abs_change)) %>%
  select(year, epi_score, change) %>%
  print()

# Visualization of changes
p5 <- change_points %>%
  ggplot(aes(x = year, y = change)) +
  geom_col(aes(fill = change > 0)) +
  scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828"),
                    labels = c("Decline", "Improvement")) +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  labs(
    title = "Year-over-Year Changes in Arkansas EPI Score",
    subtitle = "Which election cycles saw the biggest improvements or declines?",
    x = "Year",
    y = "Change from Previous Election",
    fill = ""
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("visualizations/15_year_over_year_changes.png", p5,
       width = 10, height = 6, dpi = 300)

# ============================================================================
# 8. COMPONENT ANALYSIS: Group indicators into categories
# ============================================================================

# Categorize indicators
indicator_categories <- tibble(
  indicator = unique(indicators$indicator),
  category = c(
    "Registration", "Registration", "Voting Process", "Voting Process",
    "Voting Process", "Election Workers", "Election Workers", "Absentee/Mail",
    "Accessibility", "Absentee/Mail", "Election Security", "Information",
    "Data Quality", "Registration", "Election Security", "Registration",
    "Voting Process", "Turnout"
  )
)

category_scores <- indicators %>%
  left_join(indicator_categories, by = "indicator") %>%
  group_by(year, category) %>%
  summarize(mean_score = mean(score, na.rm = TRUE), .groups = "drop")

p6 <- category_scores %>%
  ggplot(aes(x = year, y = mean_score, color = category, group = category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_viridis_d(option = "plasma") +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Performance by Category Over Time",
    subtitle = "Aggregated scores for related indicators",
    x = "Year",
    y = "Average Score",
    color = "Category"
  ) +
  theme_minimal()

ggsave("visualizations/16_category_performance.png", p6,
       width = 12, height = 6, dpi = 300)

cat("\n=== CATEGORY PERFORMANCE (2022) ===\n\n")
category_scores %>%
  filter(year == 2022) %>%
  arrange(desc(mean_score)) %>%
  mutate(mean_score = percent(mean_score, accuracy = 0.1)) %>%
  print()

cat("\n=== ADVANCED ANALYSIS COMPLETE ===\n")
cat("Additional visualizations saved to visualizations/ directory\n")
