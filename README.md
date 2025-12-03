# **Cyclistic Bike-Share Business Case**
Converting Casual Riders into Annual Members
```
                                      Author: Andre Zeni — Business Data Analyst
```
## Project Summary

This project analyses historical bike-share trip data from Cyclistic (Q1 2019 vs Q1 2020) to answer a key business question:

**How can Cyclistic increase revenue by converting more casual riders into annual members?**

Using R, I built a complete end-to-end workflow including : 
* data cleaning,
* descriptive analysis,
* behavioral segmentation,
* operational insights

### Business Impact

Cyclistic’s revenue model relies heavily on annual members.
However, ride data revealed:

* Casual rider trips grew +93% YoY (2019 → 2020)

* Member trips grew only +10.7%

* Casual riders increasingly used bikes for longer, leisure-oriented trips

**These insights indicate a substantial conversion opportunity, enabling data-driven decisions around pricing, marketing, and product design.**

## Key Insights
### 1. Growth Dynamics

* Total rides increased +15.9% YoY

* Casual segment saw exponential growth

* Casual riders represented a growing share of total trips

### 2. Behavior Differences

* Members → shorter rides, weekdays, commuting

* Casual riders → longer rides, weekends, leisure hotspots

### 3. Conversion Opportunity Segments

High-potential conversion groups:

* **Casual riders taking short/medium rides (<20 mins)**

* Casual riders riding on weekdays. *(For further analysis, it would be necessary to analyse members by its IDs, which are not available for this study)*

* These riders behave similarly to members.

### 4. Operational Insights

* Longer casual rides increase redistribution pressure

* Start/end station analysis identifies tourist vs commuter zones

* Optimising inventory and station capacity supports conversion

### Tools & Technologies

R (Tidyverse, lubridate, ggplot2)

Tableau Public (executive dashboard)

Kaggle (interactive notebook + dataset hosting)

This repo focuses on the analytical R code and documentation.

## Repository Structure

📁 cyclistic-bike-share-analysis/

```
cyclistic-bike-share-analysis/
│
├── README.md
│
├── src/
│   ├── cyclistic_analysis.R
│
├── visuals/
│   ├── growth_trend.png
│   ├── ride_distribution.png
│
├── docs/
    ├── business_case_summary.pdf
```

## **Portfolio Ecosystem :**

#### Full interactive analysis (Kaggle Notebook): *The analysis can be executed on Kaggle.*
https://www.kaggle.com/code/andrezeni/cyclistic-bike-share-q1-2019-2020

#### Executive dashboard (Tableau): 
https://public.tableau.com/app/profile/andre.zeni/vizzes

#### Full project article (Live Portfolio): 
https://andrezeni.blogspot.com/

#### LinkedIn Profile: 
https://www.linkedin.com/in/andre-zeni-fs1991/

