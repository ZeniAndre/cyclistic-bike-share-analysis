############################################################
# CYCLISTIC BUSINESS CASE - MAIN ANALYSIS SCRIPT
# Author: Andre Zeni
# Role: Business Data Analyst
# Date: 2025/12/02
############################################################

#-----------------------------------------------------------
# 0. SETUP
#-----------------------------------------------------------

# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)   # data wrangling, plots
library(lubridate)   # date/time handling

theme_set(theme_minimal())

# Set a seed for reproducibility (if ever to sample)
set.seed(123)

#-----------------------------------------------------------
# 1. DATA CLEANING AND PROCESSING
#-----------------------------------------------------------

# 1.1 Load raw data ----------------------------------------

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv", show_col_types = FALSE)
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv", show_col_types = FALSE)

# 1.2 Align schemas (make 2019 look like 2020) -------------

q1_2019 <- q1_2019 %>%
  rename(
    ride_id            = trip_id,
    rideable_type      = bikeid,
    started_at         = start_time,
    ended_at           = end_time,
    start_station_name = from_station_name,
    start_station_id   = from_station_id,
    end_station_name   = to_station_name,
    end_station_id     = to_station_id,
    member_casual      = usertype
  )

# Ensure IDs are character so they stack nicely
q1_2019 <- q1_2019 %>%
  mutate(
    ride_id       = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  )

# 1.3 Combine quarters + drop unused columns ----------------

all_trips_raw <- bind_rows(q1_2019, q1_2020) %>%
  # Drop columns removed in 2020 or not needed for this business case
  select(-c(
    start_lat, start_lng,
    end_lat,   end_lng,
    birthyear, gender,
    tripduration
  ))

# Quick sanity checks (optional)
# glimpse(all_trips_raw)
# summary(all_trips_raw)

# 1.4 Standardize rider type labels -------------------------

# 2019 uses "Subscriber"/"Customer"; 2020 uses "member"/"casual"
all_trips <- all_trips_raw %>%
  mutate(
    member_casual = recode(
      member_casual,
      "Subscriber" = "member",
      "Customer"   = "casual"
    ),
    member_casual = factor(member_casual, levels = c("member", "casual"))
  )

# table(all_trips$member_casual)

# 1.5 Derive date and time features -------------------------

all_trips <- all_trips %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at   = ymd_hms(ended_at),
    date       = as_date(started_at),
    year       = year(date),
    quarter    = quarter(date),
    month      = floor_date(date, unit = "month"),
    day_of_week = wday(date, label = TRUE, week_start = 1)  # Mon = 1
  )

# 1.6 Compute ride length (in minutes) ----------------------

all_trips <- all_trips %>%
  mutate(
    ride_length_min = as.numeric(
      difftime(ended_at, started_at, units = "mins")
    )
  )

# 1.7 Remove bad / QC data ----------------------------------

# - Negative durations
# - HQ QR quality-control station
all_trips <- all_trips %>%
  filter(
    ride_length_min >= 0,
    start_station_name != "HQ QR"
  )

# 1.8 Keep slim analysis dataset ----------------------------

all_trips <- all_trips %>%
  select(
    ride_id,
    started_at, ended_at,
    start_station_id, start_station_name,
    end_station_id,   end_station_name,
    member_casual,
    date, year, quarter, month, day_of_week,
    ride_length_min
  )

# Optional: write cleaned data to disk for reuse
# write_csv(all_trips, "all_trips_clean.csv")

#-----------------------------------------------------------
# 2. DESCRIPTIVE ANALYSIS
#   2.1 Growth dynamics
#   2.2 Ride behaviour
#   2.3 Conversion opportunity profile
#   2.4 Temporal trends
#   2.5 Operational impact
#   2.6 Key visuals
#-----------------------------------------------------------

#-----------------------------------------------------------
# 2.1 SHOW THE GROWTH DYNAMICS
#-----------------------------------------------------------

# Total rides by year and rider type
growth_by_year <- all_trips %>%
  group_by(year, member_casual) %>%
  summarise(
    rides = n(),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    total_rides   = sum(rides),
    pct_of_total  = rides / total_rides
  ) %>%
  ungroup()

View(growth_by_year)

# YoY growth (2019 -> 2020) for each segment
growth_yoy <- growth_by_year %>%
  select(year, member_casual, rides) %>%
  pivot_wider(names_from = year, values_from = rides, names_prefix = "y") %>%
  mutate(
    abs_growth = y2020 - y2019,
    pct_growth = (y2020 - y2019) / y2019
  )

View(growth_yoy)

#-----------------------------------------------------------
# 2.2 ANALYSE RIDE BEHAVIOUR
#   - Ride length distribution
#   - Weekday vs weekend patterns
#   - Differences between members and casuals
#-----------------------------------------------------------

# 2.2.1 Overall ride length summary -------------------------

ride_length_summary <- all_trips %>%
  summarise(
    mean   = mean(ride_length_min, na.rm = TRUE),
    median = median(ride_length_min, na.rm = TRUE),
    p10    = quantile(ride_length_min, 0.10, na.rm = TRUE),
    p25    = quantile(ride_length_min, 0.25, na.rm = TRUE),
    p75    = quantile(ride_length_min, 0.75, na.rm = TRUE),
    p90    = quantile(ride_length_min, 0.90, na.rm = TRUE),
    max    = max(ride_length_min, na.rm = TRUE)
  )

View(ride_length_summary)

# 2.2.2 Ride length by rider type ---------------------------

ride_length_by_type <- all_trips %>%
  group_by(member_casual) %>%
  summarise(
    rides  = n(),
    mean   = mean(ride_length_min, na.rm = TRUE),
    median = median(ride_length_min, na.rm = TRUE),
    p90    = quantile(ride_length_min, 0.90, na.rm = TRUE),
    max    = max(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  )

View(ride_length_by_type)

# 2.2.3 Bucketed distribution (for charts / Excel) ----------

breaks <- c(0, 5, 10, 15, 20, 30, 45, 60, 90, Inf)
labels <- c("0–5", "5–10", "10–15", "15–20",
            "20–30", "30–45", "45–60", "60–90", "90+")

all_trips <- all_trips %>%
  mutate(
    ride_bucket = cut(
      ride_length_min,
      breaks = breaks,
      labels = labels,
      right  = FALSE
    )
  )

dist_by_type <- all_trips %>%
  count(member_casual, ride_bucket, name = "rides")

View(dist_by_type)

# 2.2.4 Weekday behaviour -----------------------------------

weekday_behaviour <- all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    rides           = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(member_casual, day_of_week)

View(weekday_behaviour)

#-----------------------------------------------------------
# 2.3 BUILD A CONVERSION OPPORTUNITY PROFILE
#   (R side = generate the tables that support your story)
#-----------------------------------------------------------

# Example segmentation:
# - Short rides (< 15 min) vs longer rides
# - Weekday vs weekend usage

all_trips <- all_trips %>%
  mutate(
    ride_segment = case_when(
      ride_length_min < 10               ~ "short (<10 min)",
      ride_length_min >= 10 & ride_length_min < 20 ~ "medium (10–20 min)",
      TRUE                               ~ "long (20+ min)"
    ),
    is_weekend = if_else(day_of_week %in% c("Sat", "Sun"), "weekend", "weekday")
  )

conversion_profile <- all_trips %>%
  group_by(member_casual, ride_segment, is_weekend) %>%
  summarise(
    rides = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  )

View(conversion_profile)

# This table supports:
# - see which segments (e.g., casual, medium rides, weekend) might be targeted
# - Further analysis - high potential casuals vs pure tourists


#-----------------------------------------------------------
# 2.4 ANALYSE TEMPORAL TRENDS
#   - Rides over time (daily, monthly)
#   - Split by rider type
#-----------------------------------------------------------

# 2.4.1 Daily rides -----------------------------------------

daily_trends <- all_trips %>%
  group_by(date, member_casual) %>%
  summarise(
    rides           = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups         = "drop"
  )

View(daily_trends)

# 2.4.2 Monthly rides (Q1 2019 vs Q1 2020) ------------------

monthly_trends <- all_trips %>%
  group_by(month, year, member_casual) %>%
  summarise(
    rides           = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(month, member_casual)

View(monthly_trends)

#-----------------------------------------------------------
# 2.5 STUDY OPERATIONAL IMPACT
#   - Station hotspots
#   - Long rides and potential rebalancing issues
#-----------------------------------------------------------

# 2.5.1 Top start stations by rider type --------------------

top_start_stations <- all_trips %>%
  group_by(start_station_name, member_casual) %>%
  summarise(
    rides           = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(rides))

# View top 20 for quick insight
top_start_stations_20 <- top_start_stations %>%
  slice_max(order_by = rides, n = 20)

View(top_start_stations_20)

# 2.5.2 Long-ride share (potential rebalancing pressure) ----

long_ride_share <- all_trips %>%
  mutate(is_long = ride_length_min >= 30) %>%
  group_by(member_casual) %>%
  summarise(
    rides          = n(),
    long_rides     = sum(is_long),
    long_share_pct = long_rides / rides,
    .groups        = "drop"
  )

View(long_ride_share)

#-----------------------------------------------------------
# 2.6 BUILD KEY VISUALS (for slides / report)
#   These plots support your executive narrative.
#-----------------------------------------------------------

# 2.6.1 Growth dynamics: rides by year and rider type -------

ggplot(growth_by_year,
       aes(x = factor(year), y = rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Rides by Year and Rider Type (Q1)",
    x     = "Year",
    y     = "Number of rides",
    fill  = "Rider type"
  )

# 2.6.2 Ride length distribution (bucketed) -----------------

ggplot(dist_by_type,
       aes(x = ride_bucket, y = rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ride Length Distribution by Rider Type",
    x     = "Ride length (minutes)",
    y     = "Number of rides",
    fill  = "Rider type"
  )

# 2.6.3 Weekday pattern: number of rides --------------------

ggplot(weekday_behaviour,
       aes(x = day_of_week, y = rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Rides by Day of Week and Rider Type",
    x     = "Day of week",
    y     = "Number of rides",
    fill  = "Rider type"
  )

# 2.6.4 Temporal trend: daily rides -------------------------

ggplot(daily_trends,
       aes(x = date, y = rides, color = member_casual)) +
  geom_line() +
  labs(
    title = "Daily Rides Over Time (Q1 2019 vs Q1 2020)",
    x     = "Date",
    y     = "Number of rides",
    color = "Rider type"
  )
