## Cyclistic Bike-Share Business Case
Converting Casual Riders into Annual Members

Author: Andre Zeni — Business Data Analyst

### Project Summary

This project analyzes historical bike-share trip data from Cyclistic (Q1 2019 vs Q1 2020) to answer a key business question:

How can Cyclistic increase revenue by converting more casual riders into annual members?

Using R, I built a complete end-to-end workflow including data cleaning, descriptive analysis, behavioral segmentation, operational insights, and strategic recommendations for the marketing team.

This repository contains the full, reproducible analysis script and supporting documentation.

### Business Impact

Cyclistic’s revenue model relies heavily on annual members.
However, ride data revealed:

Casual rider trips grew +93% YoY (2019 → 2020)

Member trips grew only +10.7%

Casual riders increasingly used bikes for longer, leisure-oriented trips

**These insights indicate a substantial untapped conversion opportunity, enabling data-driven decisions around pricing, marketing, and product design.**

### Key Insights
#### 1. Growth Dynamics

Total rides increased +15.9% YoY

Casual segment saw exponential growth

Casual riders represented a growing share of total trips

#### 2. Behavior Differences

Members → shorter rides, weekdays, commuting

Casual riders → longer rides, weekends, leisure hotspots

#### 3. Conversion Opportunity Segments

High-potential conversion groups:

Casual riders taking short/medium rides (<20 mins)

Casual riders riding on weekdays

These riders behave similarly to members.

#### 4. Operational Insights

Longer casual rides increase redistribution pressure

Start/end station analysis identifies tourist vs commuter zones

Optimizing inventory and station capacity supports conversion

### Tools & Technologies

R (Tidyverse, lubridate, ggplot2)

data.table (optional performance version included)

Tableau Public (executive dashboard)

Kaggle (interactive notebook + dataset hosting)

This repo focuses on the analytical R code and documentation.

## Repository Structure

📁 cyclistic-bike-share-analysis/
│
├── cyclistic_analysis.R               # Cleaned, structured R script
├── cyclistic_analysis_datatable.R     # Optional optimized version using data.table
├── README.md                          # This file
│
├── /visuals                           # Exported charts (optional)
├── /data                              # Cleaned data (if allowed to share)
└── /docs                              # Additional documentation

### How to Reproduce the Analysis

1. Clone the repo

2. Install R packages:
install.packages(c("tidyverse", "lubridate"))

3. Run:
source("cyclistic_analysis.R")

4. Outputs appear in the Viewer and console.

## Related Links (Portfolio Ecosystem)
### Full interactive analysis (Kaggle Notebook): https://www.kaggle.com/code/andrezeni/cyclistic-bike-share-q1-2019-2020

### Executive dashboard (Tableau): [Tableau]https://public.tableau.com/app/profile/andre.zeni/vizzes

### Full project article (Live Portfolio): [Andre Zeni](https://andrezeni.blogspot.com/)

### LinkedIn Profile: https://www.linkedin.com/in/andre-zeni-fs1991/
