

# Task 4 - 2017 Data Clean

# Load Data and Libraries ----

bb_candy_2017 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2017.xlsx")

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(here)
library(dplyr)
library(assertr)

# Convert All Data to Lowercase ----

bb_candy_2017_cleaned <- bb_candy_2017 %>%
  mutate_all(str_to_lower) %>%
  clean_names()

# Select All Relevant Columns ----

colnames(bb_candy_2017_cleaned)

bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
  select(-internal_id,
         -q5_state_province_county_etc,
         -q6_real_housewives_of_orange_county_season_9_blue_ray,
         -q7_joy_other,
         -q8_despair_other,
         -q9_other_comments,
         -q10_dress,
         -x114,
         -q11_day,
         -q12_media_daily_dish,
         -q12_media_science,
         -q12_media_espn,
         -q12_media_yahoo,
         -click_coordinates_x_y
  )
         
colnames(bb_candy_2017_cleaned)       


# Pivot Data Set to Long Format ----

bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
  pivot_longer(
    cols = "q6_100_grand_bar" : "q6_york_peppermint_patties",
    names_to = "item_gifted",
    values_to = "reaction"
  )        

# Rename Columns ----

bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
  rename(
    age = q3_age,
    trick_or_treat = q1_going_out,
    gender = q2_gender,
    country = q4_country
  )    

# Write Data To Clean Folder For Later Use ----

write_csv(bb_candy_2017_cleaned, "clean_data/bb_candy_2017_cleaned")

# End ----
         
         




