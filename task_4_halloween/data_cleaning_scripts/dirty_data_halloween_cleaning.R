# 1.4 Task 4 - Halloween Candy Data ----

# Step 1 - Load Libraries ----

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(here)
library(dplyr)
library(assertr)

here::here()

# Step 2 - Load Data 

bb_candy_2015 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2015.xlsx")
bb_candy_2016 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2016.xlsx")
bb_candy_2017 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2017.xlsx")

# First Data Set - bb_candy_2015 ----

# convert all to lower case

bb_candy_2015_cleaned <- bb_candy_2015 %>%
  mutate_all(str_to_lower) %>%
  clean_names()

# pivot to long

bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
  pivot_longer(
    cols = "butterfinger" : "york_peppermint_patties",
    names_to = "item_gifted",
    values_to = "reaction"
  )
  
# rename columns

 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   rename(
     date = timestamp,
     age = how_old_are_you,
     trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself
   )
   
 # select relevant columns
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   select(date, age, trick_or_treat, item_gifted, reaction)

 # change column class
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   mutate(date = as.Date(date),
          age = as.integer(age))
 
 # change column 'trick or treat' to true or false, then to logical
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   mutate(
     trick_or_treat = recode(trick_or_treat, "yes" = TRUE, "no" = FALSE)
   )
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   mutate(trick_or_treat = as.logical(trick_or_treat))

 
str(bb_candy_2015_cleaned)


 # remove the time from the date column
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
   mutate(date = str_remove(
       date, pattern = " \\[0-9](:)"))
 
 # remove NA from rating
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>% 
   drop_na()

 # sort out ages
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>% 
   mutate(
     age = case_when(
       age <= 6 ~ 0,
       age >= 100 ~ 0,
       TRUE ~ as.integer(age)
       )
   )
 
   
 
 
 
 
 
 
 
 



