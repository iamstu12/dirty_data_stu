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
 
 # remove NA from reaction
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
    drop_na(reaction)


 # sort out ages
 
 bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>% 
   mutate(
     age = case_when(
       age >= 6 & age <= 100 ~ as.integer(age)
       )
   )

 bb_candy_2015_cleaned %>%
    summarise(count = sum(is.na(age)))
 
 
# Second Data Set - bb_candy 2016 ----

# convert all to lower case
 
 bb_candy_2016_cleaned <- bb_candy_2016 %>%
    mutate_all(str_to_lower) %>%
    clean_names()
 
 # pivot to long
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    pivot_longer(
       cols = "x100_grand_bar" : "york_peppermint_patties",
       names_to = "item_gifted",
       values_to = "reaction"
    )
 
 # rename columns
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    rename(
       date = timestamp,
       age = how_old_are_you,
       trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself,
       gender = your_gender,
       country = which_country_do_you_live_in
    )
 
 # select relevant columns
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    select(date, age, trick_or_treat, item_gifted, reaction,
           gender, country, state)
 
 # change the class of columns
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    mutate(date = as.Date(date),
           age = as.integer(age))
 
 # change column 'trick or treat' to true or false, then to logical
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    mutate(
       trick_or_treat = recode(trick_or_treat, "yes" = TRUE, "no" = FALSE)
    )
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    mutate(trick_or_treat = as.logical(trick_or_treat))
 
 
 str(bb_candy_2016_cleaned)
 
 # remove the time from the date column
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    mutate(date = str_remove(
       date, pattern = " \\[0-9](:)"))
 
 # remove NA from reaction
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
    drop_na(reaction)
 
 # sort out ages
 
 bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>% 
    mutate(
       age = case_when(
          age >= 6 & age <= 100 ~ as.integer(age)
       )
    )
 
 bb_candy_2016_cleaned %>%
    summarise(count = sum(is.na(age)))
 
# Third Data Set - bb_candy 2017 ----
 
# convert all to lower case
 
 bb_candy_2017_cleaned <- bb_candy_2017 %>%
    mutate_all(str_to_lower) %>%
    clean_names()
 
 # remove 'q' from column title
 
 colnames(bb_candy_2017_cleaned) <- 
    str_remove_all(colnames(bb_candy_2017_cleaned), "[q][0-9][_]")
 
 # pivot to longer
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    pivot_longer(
       cols = "100_grand_bar" : "york_peppermint_patties",
       names_to = "item_gifted",
       values_to = "reaction"
    )
 
 # rename columns
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    rename(
       trick_or_treat = going_out
    )
 
 # select relevant columns
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    select(age, trick_or_treat, item_gifted, reaction,
           gender, country)
 
 # change the class of columns
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    mutate(age = as.integer(age))
 
 # change column 'trick or treat' to true or false, then to logical
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    mutate(
       trick_or_treat = recode(trick_or_treat, "yes" = TRUE, "no" = FALSE)
    )
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    mutate(trick_or_treat = as.logical(trick_or_treat))
 
 # drop missing values from reaction
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    drop_na(reaction)
 
 # clean names
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>% 
    mutate(
       age = case_when(
          age >= 6 & age <= 100 ~ as.integer(age)
       )
    )
 
 bb_candy_2017_cleaned %>%
    summarise(count = sum(is.na(age)))
 
# clean gender column ----
 
 bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>% 
    mutate(
       gender = case_when(
          gender == "male" ~ "male",
          gender == "female" ~ "female",
          gender == "other" ~ "other",
          TRUE ~ "undisclosed"
          )
    )

# clean country names ----

bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
   mutate(
      country = case_when(
         country == "usa" ~ "united states",
         country == "us" ~ "united states",
         country == "murica" ~ "united states",
         country == "united staes" ~ "united states",
         country == "united states of america" ~ "united states",
         country == "uae" ~ "united states",
         country == "england" ~ "europe",
         country == "u.s.a." ~ "united states",
         country == "usausausa" ~ "united states",
         country == "america" ~ "united states",
         country == "france" ~ "europe",
         country == "mexico" ~ "united states",
         country == "us of a" ~ "united states",
         country == "unites states" ~ "united states",
         country == "the united states" ~ "united states",
         country == "north carolina" ~ "united states",
         country == "netherlands" ~ "europe",
         country == "europe" ~ "europe",
         country == "earth" ~ "undisclosed",
         country == "u s" ~ "united states",
         country == "u.s." ~ "united states",
         country == "costa rica" ~ "united states",
         country == "the united states of america" ~ "united states",
         country == "cascadia" ~ "united states",
         country == "insanity lately" ~ "undisclosed",
         country == "greece" ~ "europe",
         country == "usa? hard to tell anymore.." ~ "united states",
         country == "'merica" ~ "united states",
         country == "pittsburgh" ~ "united states",
         country == "united state" ~ "united states",
         country == "a" ~ "united states",
         country == "can" ~ "canada",
         country == "canae" ~ "united states",
         country == "new york" ~ "united states",
         country == "trumpistan" ~ "united states",
         country == "ireland" ~ "europe",
         country == "united sates" ~ "united states",
         country == "korea" ~ "asia",
         country == "australia" ~ "australia",
         country == "california" ~ "united states",
         country == "japan" ~ "asia",
         country == "south africa" ~ "africa",
         country == "i pretend to be from canada, but i am really from the united states." ~ "united states",
         country == "iceland" ~ "europe",
         country == "canada`" ~ "canada",
         country == "scotland" ~ "europe",
         country == "denmark" ~ "europe",
         country == "switzerland" ~ "europe",
         country == "ahem....amerca" ~ "united states",
         country == "south korea" ~ "asia",
         country == "new jersey" ~ "united states",
         country == "united stated" ~ "united states",
         country == "germany" ~ "europe",
         country == "united statss" ~ "united states",
         country == "endland" ~ "undisclosed",
         country == "atlantis" ~ "undisclosed",
         country == "murrika" ~ "united states",
         country == "alaska" ~ "united states",
         country == "n. america" ~ "united states",
         country == "singapore" ~ "asia",
         country == "ussa" ~ "united states",
         country == "taiwan" ~ "asia",
         country == "china" ~ "asia",
         country == "spain" ~ "europe",
         country == "narnia" ~ "undisclosed",
         country == "u s a"  ~ "united states",
         country == "united statea" ~ "united states",
         country == "subscribe to dm4uz3 on youtube" ~ "undisclosed",
         country == "usa usa usa!!!!" ~ "united states",
         country == "i don't know anymore" ~ "undisclosed",
         country == "fear and loathing" ~ "undisclosed",
         TRUE ~ "undisclosed"))
 
# clean up items_gifted ----
 
bb_candy_2017_cleaned <- bb_candy_2017_cleaned %>%
    mutate(item_gifted = str_replace_all(
       item_gifted, pattern = "_", " "
    ))
 

 
 

 
 
 
 
 
 

  
 
 
 
   
 
 
 
 
 
 
 
 




 