# Arkansas Elections Performance Index Analysis

This repository contains data and R code for analyzing election administration in Arkansas using the MIT Elections Performance Index (EPI).

## About the Elections Performance Index

The EPI is a comprehensive measure of election administration quality developed by MIT. It examines multiple dimensions including:
- Voter registration processes
- Voting convenience (wait times, early voting)
- Vote counting accuracy (residual votes)
- Accessibility (military, disability)
- Information provision (websites)
- Election security (audits)

## Data Files

Located in `data/`:

- **epi_arkansas_indicators.csv**: Arkansas scores for 18 indicators (2008-2022)
- **epi_arkansas_rankings.csv**: Arkansas national ranking over time
- **epi_scores_comparison.csv**: Arkansas vs. National Average
- **epi_regional_comparison.csv**: Arkansas vs. neighboring states
- **epi indicators-all years.csv**: Full dataset with all states (2008-2022)

## Key Findings

### Overall Performance
- **2008**: Score of 48.1 (ranked #42 of 51)
- **2022**: Score of 56.1 (ranked #35 of 51)
- **Improvement**: +8 points, +7 positions over 14 years

### Major Improvements
1. **Online Registration**: 0% → 95% (adopted 2020)
2. **Post-Election Audits**: 0% → 85% (implemented 2018)
3. **Turnout**: +6.7 percentage points
4. **Absentee Rejection**: +0.7 points (fewer rejections)

### Areas of Concern
1. **ERIC Membership**: Remains at 0% (not a member)
2. **Risk-Limiting Audits**: 0% (not implemented)
3. **Registration Accuracy**: Slight decline (-1.4 points)
4. **Disability Access**: Declined by -12.4 points

### Regional Context
Among neighboring states (2022):
1. Tennessee: 82
2. Missouri: 81
3. Louisiana: 74
4. **Arkansas: 56**
5. Mississippi: 64

Arkansas trails all neighbors except Mississippi.

## Visualizations Created

The `arkansas_epi_analysis.R` script generates 10 publication-quality visualizations:

1. **Arkansas vs National Average** - Trend line comparison showing the gap
2. **Regional Comparison** - Arkansas vs. neighboring states
3. **Ranking Trajectory** - Arkansas's rank among 51 states over time
4. **Indicator Heatmap** - Performance across all 18 indicators
5. **2022 Strengths & Weaknesses** - Current performance bar chart
6. **14-Year Changes** - Which indicators improved/declined most
7. **Key Indicators Deep Dive** - Time series for critical metrics
8. **2008 vs 2022 Scatter** - Before/after comparison
9. **Policy Adoption Timeline** - When Arkansas implemented reforms
10. **Score Decomposition** - What contributes to overall score

## Running the Analysis

### Prerequisites
```r
install.packages(c("tidyverse", "scales", "viridis", "patchwork", "ggrepel"))
```

### Execute
```r
# Set working directory to repository root
setwd("path/to/Arkansas-elections")

# Run main analysis
source("arkansas_epi_analysis.R")

# Visualizations will be saved to visualizations/
```

## R Scripts

- **arkansas_epi_analysis.R**: Main analysis with 10 visualizations
- **advanced_analysis.R**: Statistical modeling and deeper analysis (coming soon)

## Interpretation Guide

### Indicator Scores
- **0.75-1.00**: Strong performance
- **0.50-0.74**: Moderate performance
- **0.00-0.49**: Weak performance / needs improvement

### EPI Total Score
Arkansas's EPI score is a composite of 18 indicators. The national average has increased from 58 (2008) to 80 (2022), indicating nationwide improvements in election administration. Arkansas's slower improvement rate means it's falling further behind the national average.

## Policy Implications

### Quick Wins
1. **Join ERIC**: Electronic Registration Information Center for list maintenance
2. **Implement Risk-Limiting Audits**: Statistical method to verify results
3. **Improve Registration Accuracy**: Update list maintenance procedures

### Long-term Improvements
1. **Reduce Wait Times**: Better poll worker training and resource allocation
2. **Enhance Disability Access**: Ensure accessible voting locations
3. **Lower Residual Vote Rate**: Ballot design and voting technology upgrades

## Research Questions for Further Analysis

1. Does Arkansas's rural population affect certain metrics (e.g., wait times)?
2. What specific policies led to the 2018-2020 improvement?
3. How do county-level variations affect state performance?
4. What is the fiscal impact of implementing ERIC or RLAs?
5. Are there partisan patterns in which states excel at different indicators?

## Data Sources

- MIT Elections Performance Index: https://elections.mit.edu
- Methodology: MIT Election Data and Science Lab
- Years covered: 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022

## License

Data from MIT Elections Performance Index. Visualization code released under MIT License.

## Contact

For questions about Arkansas election administration:
- Arkansas Secretary of State: https://www.sos.arkansas.gov/

For questions about the EPI methodology:
- MIT Election Lab: https://elections.mit.edu/
