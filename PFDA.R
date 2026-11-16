# ===== Group G39 =====
# William Jonathan (TP075381), Teddy Steven Gotama (TP086582), ...

# Load packages
library(tidyverse)
library(lubridate)
library(zoo)

# Import dataset
df <- read.csv("Beijing_AirQuality.csv")

# ===== 2. Data Preparation =====
# 2.1 Initial inspection: structure, summary, missing values, station and wind labels, duplicates
dim(df)
str(df)
head(df)
summary(df)
colSums(is.na(df))
table(df$station)
table(df$wd, useNA = "ifany")
sum(duplicated(df))

# 2.2 Standardise station names to title case and wind direction to upper case
df <- df %>%
  mutate(
    station = str_to_title(station),
    wd      = toupper(wd)
  )
table(df$station)
table(df$wd, useNA = "ifany")

# 2.3 Remove row number column and exact duplicate rows
df <- df %>% select(-No)
sum(duplicated(df))
df <- distinct(df)
sum(duplicated(df))
dim(df)

# 2.4 Remove repeated readings for the same station and hour, keeping the first
sum(duplicated(df[, c("station", "year", "month", "day", "hour")]))
df <- df %>% distinct(station, year, month, day, hour, .keep_all = TRUE)
sum(duplicated(df[, c("station", "year", "month", "day", "hour")]))
dim(df)

# 2.5 Remove rows with invalid hour (above 23), replace impossible values with NA: placeholder temperature, negative wind speed, extreme ozone
table(df$hour[df$hour > 23])
df <- df %>% filter(hour >= 0 & hour <= 23)
range(df$hour)

sum(df$TEMP == -999, na.rm = TRUE)
sum(df$WSPM < 0, na.rm = TRUE)
sum(df$O3 > 500, na.rm = TRUE)
df <- df %>%
  mutate(
    TEMP = ifelse(TEMP == -999, NA, TEMP),
    WSPM = ifelse(WSPM < 0, NA, WSPM),
    O3   = ifelse(O3 > 500, NA, O3)
  )
summary(df[, c("TEMP", "WSPM", "O3")])

# 2.6 Create datetime column and sort by station and time
df <- df %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  arrange(station, datetime)
head(df)

# 2.7 Handle missing values: complete the hourly timeline per station, interpolate gaps of up to 24 hours, fill wind direction per station, then drop rows without PM2.5 or with unfillable gaps
colSums(is.na(df))
df <- df %>%
  select(-year, -month, -day, -hour) %>%
  group_by(station) %>%
  complete(datetime = seq(min(datetime), max(datetime), by = "hour")) %>%
  arrange(station, datetime)
nrow(df)

num_cols <- c("PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "DEWP", "RAIN", "WSPM")
df <- df %>%
  mutate(across(all_of(num_cols), ~ na.approx(.x, maxgap = 24, na.rm = FALSE))) %>%
  fill(wd, .direction = "downup") %>%
  ungroup()

sum(is.na(df$PM2.5))
df <- df %>% filter(!is.na(PM2.5))
colSums(is.na(df))
df <- df %>% drop_na()
colSums(is.na(df))

df <- df %>%
  mutate(
    year  = year(datetime),
    month = month(datetime),
    day   = day(datetime),
    hour  = hour(datetime)
  ) %>%
  relocate(year, month, day, hour, .before = PM2.5)
dim(df)

# 2.8 Flag readings where PM2.5 is greater than PM10 (physically impossible, kept but flagged)
df <- df %>% mutate(pm25_gt_pm10 = PM2.5 > PM10)
table(df$pm25_gt_pm10)

# 2.9 Add shared columns: season, heating season, station type
df <- df %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE                   ~ "Autumn"
    ),
    season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
    heating_season = (month %in% c(12, 1, 2)) | (month == 11 & day >= 15) | (month == 3 & day <= 15),
    station_type = ifelse(station %in% c("Dongsi", "Gucheng", "Wanliu"), "Urban", "Suburban")
  )
table(df$season)
table(df$heating_season)
table(df$station, df$station_type)

# 2.10 Final validation
dim(df)
str(df)
summary(df)
sum(duplicated(df[, c("station", "datetime")]))

# Save cleaned dataset
write.csv(df, "beijing_clean.csv", row.names = FALSE)
