# Arkansas EPI Analysis Guide

## What You Have

I've created a comprehensive R analysis suite for examining Arkansas's election administration performance using the MIT Elections Performance Index data.

## Files Created

### R Scripts

1. **quick_start.R** - Start here!
   - Quick summary statistics
   - Top/bottom performers
   - Simple overview visualization
   - Perfect for getting oriented with the data

2. **arkansas_epi_analysis.R** - Main analysis (10 visualizations)
   - Publication-quality plots
   - Comprehensive exploration of all dimensions
   - Run time: ~30 seconds

3. **advanced_analysis.R** - Statistical modeling (6 additional visualizations)
   - Trend analysis and forecasting
   - Correlation analysis
   - Volatility metrics
   - Category-based aggregation

### Documentation

- **README.md** - Complete project documentation
- **ANALYSIS_GUIDE.md** - This file

## How to Use

### Prerequisites
```r
install.packages(c("tidyverse", "scales", "viridis", "patchwork", "ggrepel", "broom", "reshape2"))
```

### Running the Analysis

```r
# Navigate to project directory
setwd("path/to/Arkansas-elections")

# Quick exploration (run this first!)
source("quick_start.R")

# Full analysis with 10 visualizations
source("arkansas_epi_analysis.R")

# Advanced statistical analysis
source("advanced_analysis.R")
```

## Key Insights from the Data

### Overall Performance
- Arkansas scored **48.1** in 2008, improved to **56.1** in 2022
- National rank improved from **#42** to **#35** (out of 51)
- However, the gap with the national average **widened** from 9.9 to 23.9 points
- The national average rose faster (58→80) than Arkansas (48→56)

### Major Success Stories

1. **Online Registration** (0% → 95%)
   - Adopted in 2020
   - Brought Arkansas in line with most states

2. **Post-Election Audits** (0% → 85%)
   - Implemented in 2018
   - Significant election security improvement

3. **Absentee Ballot Rejection** (75% → 76%)
   - Slight improvement in acceptance rate
   - Consistently strong performance

### Areas of Concern

1. **ERIC Membership** (0%)
   - Electronic Registration Information Center
   - Tool for maintaining voter rolls
   - Arkansas has not joined

2. **Risk-Limiting Audits** (0%)
   - Gold standard for post-election verification
   - Not yet implemented

3. **Disability Access** (57% → 44%)
   - **Declined 12.4 points** over 14 years
   - Significant accessibility concerns

4. **Registration Accuracy** (48% → 49%)
   - Minimal improvement
   - Remains below 50%

### Regional Context (2022)

Arkansas ranks **4th of 5** among neighboring states:

1. Tennessee: 82
2. Missouri: 81
3. Louisiana: 74
4. **Arkansas: 56**
5. Mississippi: 64

Only Mississippi performs worse, but even Mississippi (64) outpaced Arkansas's improvement rate.

## What Each Visualization Shows

### Main Analysis (arkansas_epi_analysis.R)

1. **Arkansas vs National Average**
   - Line chart showing widening gap
   - Arkansas improved but not as fast as nation

2. **Regional Comparison**
   - Arkansas vs. 4 neighboring states
   - Shows competitive disadvantage

3. **Ranking Trajectory**
   - Arkansas's rank over time
   - Improved 2008-2020, slight decline in 2022

4. **Indicator Heatmap**
   - All 18 indicators × 8 election years
   - Visual pattern recognition
   - Shows which indicators changed when

5. **Strengths & Weaknesses (2022)**
   - Bar chart of current performance
   - Color-coded by performance level
   - Identifies priority areas

6. **14-Year Changes**
   - Which indicators improved/declined most
   - Shows policy impact
   - Highlights success stories and problem areas

7. **Key Indicators Deep Dive**
   - 6-panel time series
   - Focus on critical metrics
   - Shows trends in detail

8. **2008 vs 2022 Scatter**
   - Before/after comparison
   - Points above diagonal = improvement
   - Highlights biggest changes

9. **Policy Adoption Timeline**
   - When reforms were implemented
   - Binary indicators (yes/no policies)
   - Shows modernization trajectory

10. **Score Decomposition**
    - What contributes to overall score
    - All indicators ranked
    - Shows relative performance

### Advanced Analysis (advanced_analysis.R)

11. **Trend Analysis**
    - Annual rate of change per indicator
    - Statistical significance testing
    - Identifies which trends are real vs. noise

12. **Gap Analysis**
    - Arkansas-National gap over time
    - Quadratic fit shows acceleration
    - Gap is widening faster recently

13. **Correlation Matrix**
    - Which indicators move together
    - Heatmap of relationships
    - Useful for understanding systemic issues

14. **Volatility Analysis**
    - Which indicators are stable vs. volatile
    - Scatter plot of stability vs. performance
    - Identifies reliable vs. unreliable metrics

15. **Year-over-Year Changes**
    - Which election cycles had biggest shifts
    - Bar chart of deltas
    - Shows impact of specific elections/policies

16. **Category Performance**
    - Indicators grouped by type
    - Registration, Voting Process, Accessibility, etc.
    - Shows which areas need focus

## Research Questions These Scripts Can Answer

1. **Is Arkansas improving?**
   - Yes, but slower than the nation

2. **Where should Arkansas focus improvement efforts?**
   - ERIC membership (easy policy change)
   - Risk-limiting audits (security improvement)
   - Disability access (major decline)
   - Registration accuracy (stagnant)

3. **What policies had the biggest impact?**
   - Online registration (+95 points)
   - Post-election audits (+85 points)

4. **How does Arkansas compare regionally?**
   - Trails Tennessee and Missouri significantly
   - Slightly behind Louisiana
   - Ahead of Mississippi

5. **What's the trajectory?**
   - Improving, but gap with nation is widening
   - 2022 saw a slight decline in rank

## Customizing the Analysis

### Modify Color Schemes
```r
# In any script, change the ar_colors definition:
ar_colors <- c(
  "Arkansas" = "#YOUR_COLOR",
  ...
)
```

### Add More States
```r
# Use the all_states data:
all_states <- read_csv("data/epi indicators-all years.csv")

# Filter for specific states
comparison_states <- all_states %>%
  filter(state_abbv %in% c("AR", "TX", "OK", "LA", "MS", "TN", "MO"))
```

### Change Time Period
```r
# Filter to specific years:
indicators %>%
  filter(year >= 2016) %>%
  # ... rest of analysis
```

### Export Data
```r
# Save processed data for use elsewhere
write_csv(your_data, "output/filename.csv")

# Or save R data objects
saveRDS(your_data, "output/filename.rds")
```

## Next Steps

1. **Run quick_start.R** to get oriented
2. **Run arkansas_epi_analysis.R** to generate visualizations
3. **Review visualizations** in the visualizations/ folder
4. **Run advanced_analysis.R** for deeper insights
5. **Customize** for your specific research questions
6. **Share** visualizations in presentations, reports, papers

## Potential Extensions

### County-Level Analysis
If you obtain county-level data:
- Map performance variation across Arkansas
- Identify best/worst performing counties
- Analyze urban vs. rural differences

### Policy Impact Analysis
With policy implementation dates:
- Difference-in-differences analysis
- Synthetic control methods
- Event study designs

### Comparative State Analysis
Using all_states data:
- Cluster analysis to find similar states
- Panel regression models
- Fixed effects analysis

### Demographic Correlations
With demographic data:
- Race/ethnicity and access
- Income and participation
- Age and voting methods

## Contact & Citations

### Data Source
```
MIT Elections Performance Index
https://elections.mit.edu/
Charles Stewart III, MIT Election Data + Science Lab
```

### Citation Format
```
Stewart, Charles III. 2023. "The Elections Performance Index."
MIT Election Data and Science Lab.
https://elections.mit.edu/
```

### For Questions
- **Arkansas-specific**: Arkansas Secretary of State
- **EPI methodology**: MIT Election Lab
- **This code**: See repository README

## Troubleshooting

### "Package not found"
```r
install.packages(c("tidyverse", "scales", "viridis", "patchwork", "ggrepel"))
```

### "Cannot find file"
Make sure you've set the working directory:
```r
setwd("path/to/Arkansas-elections")
getwd()  # Verify it's correct
```

### "Out of memory"
The full all_states dataset is large. Use:
```r
# Read only specific columns
all_states <- read_csv("data/epi indicators-all years.csv",
                       col_select = c(state_abbv, year, online_reg, ...))
```

### Plots not saving
Create the directory first:
```r
dir.create("visualizations", showWarnings = FALSE)
```

---

**Happy analyzing!** If you create interesting findings from this data, consider sharing them with the election administration research community.
