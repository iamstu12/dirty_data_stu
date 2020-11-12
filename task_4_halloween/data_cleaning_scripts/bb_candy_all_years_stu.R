

# Task 4 - 2015 - 2017 Data Clean

# Load Data and Libraries ----

bb_candy_2015_cleaned <- read_csv("/Users/Natifu/dirty_data_stu/task_4_halloween/clean_data/bb_candy_2015_cleaned")
bb_candy_2016_cleaned <- read_csv("/Users/Natifu/dirty_data_stu/task_4_halloween/clean_data/bb_candy_2016_cleaned")
bb_candy_2017_cleaned <- read_csv("/Users/Natifu/dirty_data_stu/task_4_halloween/clean_data/bb_candy_2017_cleaned")


library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(here)
library(dplyr)
library(assertr)

# Join Tables ----

bb_candy_all_cleaned <- bind_rows(bb_candy_2015_cleaned, bb_candy_2016_cleaned,
                                  bb_candy_2017_cleaned)

# change order of columns

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  select(date, age, gender, country, trick_or_treat, item_gifted, reaction)

# Date Column ----

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(date = as.Date(date)) %>%
  mutate(date = str_remove(
    date, pattern = " \\[0-9](:)"))

# Age Column ----

bb_candy_all_cleaned <- bb_candy_all_cleaned %>% 
  mutate(
    age = case_when(
      age >= 6 & age <= 100 ~ as.integer(age)
    )
  )

bb_candy_all_cleaned %>%
  summarise(count = sum(is.na(age)))

# Gender Column ----

unique(bb_candy_all_cleaned$gender)

bb_candy_all_cleaned <- bb_candy_all_cleaned %>% 
  mutate(
    gender = case_when(
      gender == "male" ~ "male",
      gender == "female" ~ "female",
      gender == "other" ~ "other",
      gender == "i'd rather not say" ~ "undisclosed",
      TRUE ~ "undisclosed"
      )
)

unique(bb_candy_all_cleaned$gender)

# Country Column ----

unique(bb_candy_all_cleaned$country)

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(country =
    str_remove_all(country, "[[:punct:]]+")) %>%
  mutate(
    country = case_when(
      str_detect(country, "canada") ~ "canada",
      str_detect(country, "usa") ~ "united states",
      str_detect(country, "us") ~ "united states",
      str_detect(country, "uk") ~ "europe",
      str_detect(country, "united states of america") ~ "united states",
      str_detect(country, "japan") ~ "asia",
      str_detect(country, "united states") ~ "united states",
      str_detect(country, "france") ~ "europe",
      str_detect(country, "ussa") ~ "united states",
      str_detect(country, "a tropical island south of the equator") ~ "undisclosed",
      str_detect(country, "england") ~ "europe",
      str_detect(country, "switzerland") ~ "europe",
      str_detect(country, "murica") ~ "united states",
      str_detect(country, "united kingdom") ~ "europe",
      str_detect(country, "neverland") ~ "undisclosed",
      str_detect(country, "usa!") ~ "united states",
      str_detect(country, "this one") ~ "undisclosed",
      str_detect(country, "usa (i think but it's an election year so who can really tell)") ~ "united states",
      str_detect(country, "korea") ~ "asia",
      str_detect(country, "51.0") ~ "undisclosed",
      str_detect(country, "u.s.") ~ "united states",
      str_detect(country, "america") ~ "united states",
      str_detect(country, "units states") ~ "united states",
      str_detect(country, "belgium") ~ "europe",
      str_detect(country, "croatia") ~ "europe",
      str_detect(country, "portugal") ~ "europe",
      str_detect(country, "usa usa usa") ~ "united states",
      str_detect(country, "the best one - usa") ~ "united states",
      str_detect(country, "usa! usa! usa!") ~ "united states",
      str_detect(country, "47.0") ~ "undisclosed",
      str_detect(country, "cascadia") ~ "united states",
      str_detect(country, "españa") ~ "europe",
      str_detect(country, "there isn't one for old men") ~ "undisclosed",
      str_detect(country, "panama") ~ "central america",
      str_detect(country, "one of the best ones") ~ "undisclosed",
      str_detect(country, "the yoo ess of aaayyyyyy") ~ "united states",
      str_detect(country, "united kindom") ~ "europe",
      str_detect(country, "australia") ~ "australia",
      str_detect(country, "hungary") ~ "europe",
      str_detect(country, "austria") ~ "europe",
      str_detect(country, "somewhere") ~ "undisclosed",
      str_detect(country, "new zealand") ~ "new zealand",
      str_detect(country, "54.0") ~ "undisclosed",
      str_detect(country, "germany") ~ "europe",
      str_detect(country, "mexico") ~ "north america",
      str_detect(country, "44.0") ~ "undisclosed",
      str_detect(country, "brasil") ~ "south america",
      str_detect(country, "god's country") ~ "undisclosed",
      str_detect(country, "south korea") ~ "asia",
      str_detect(country, "usa!!!!!!") ~ "united states",
      str_detect(country, "philippines") ~ "asia",
      str_detect(country, "eua") ~ "undisclosed",
      str_detect(country, "usa! usa!") ~ "united states",
      str_detect(country, "45.0") ~ "undisclosed",
      str_detect(country, "sweden") ~ "europe",
      str_detect(country, "united sates") ~ "united states",
      str_detect(country, "sub-canadian north america... 'merica") ~ "united states",
      str_detect(country, "the netherlands") ~ "europe",
      str_detect(country, "finland") ~ "europe",
      str_detect(country, "trumpistan") ~ "united states",
      str_detect(country, "merica") ~ "united states",
      str_detect(country, "china") ~ "asia",
      str_detect(country, "see above") ~ "undisclosed",
      str_detect(country, "kenya") ~ "africa",
      str_detect(country, "30.0") ~ "undisclosed",
      str_detect(country, "netherlands") ~ "europe",
      str_detect(country, "the republic of cascadia") ~ "united states",
      str_detect(country, "united stetes") ~ "united states",
      str_detect(country, "not the usa or canada") ~ "canada",
      str_detect(country, "usa usa usa usa") ~ "united states",
      str_detect(country, "united  states of america") ~ "united states",
      str_detect(country, "denial") ~ "undisclosed",
      str_detect(country, "united state") ~ "united states",
      str_detect(country, "united staes") ~ "united states",
      str_detect(country, "uae") ~ "undisclosed",
      str_detect(country, "usausausa") ~ "united states",
      str_detect(country, "35") ~ "undisclosed",
      str_detect(country, "unhinged states") ~ "undisclosed",
      str_detect(country, "us of a") ~ "united states",
      str_detect(country, "unites states") ~ "united states",
      str_detect(country, "the united states") ~ "united states",
      str_detect(country, "north carolina") ~ "united states",
      str_detect(country, "unied states") ~ "united states",
      str_detect(country, "europe") ~ "europe",
      str_detect(country, "earth") ~ "undisclosed",
      str_detect(country, "u s") ~ "united states",
      str_detect(country, "u.k.") ~ "europe",
      str_detect(country, "costa rica") ~ "north america",
      str_detect(country, "the united states of america") ~ "united states",
      str_detect(country, "unite states") ~ "united states",
      str_detect(country, "46") ~ "united states",
      str_detect(country, "insanity lately") ~ "undisclosed",
      str_detect(country, "greece") ~ "europe",
      str_detect(country, "usa? hard to tell anymore..") ~ "united states",
      str_detect(country, "'merica") ~ "united states",
      str_detect(country, "usas") ~ "united states",
      str_detect(country, "pittsburgh") ~ "united states",
      str_detect(country, "45") ~ "undisclosed",
      str_detect(country, "32") ~ "undisclosed",
      str_detect(country, "a") ~ "undisclosed",
      str_detect(country, "can") ~ "canada",
      str_detect(country, "canae") ~ "canada",
      str_detect(country, "new york") ~ "united states",
      str_detect(country, "ireland") ~ "europe",
      str_detect(country, "california") ~ "united states",
      str_detect(country, "south africa") ~ "south africa",
      str_detect(country, "i pretend to be from canada, but i am really from the united states.") ~ "united states",
      str_detect(country, "iceland") ~ "europe",
      str_detect(country, "canada`") ~ "canada",
      str_detect(country, "scotland") ~ "europe",
      str_detect(country, "denmark") ~ "europe",
      str_detect(country, "united stated") ~ "united states",
      str_detect(country, "ahem....amerca") ~ "united states",
      str_detect(country, "ud") ~ "united states",
      str_detect(country, "new jersey") ~ "united states",
      str_detect(country, "indonesia") ~ "asia",
      str_detect(country, "united ststes") ~ "united states",
      str_detect(country, "united statss") ~ "united states",
      str_detect(country, "endland") ~ "undisclosed",
      str_detect(country, "atlantis") ~ "undisclosed",
      str_detect(country, "murrika") ~ "united states",
      str_detect(country, "usaa") ~ "united states",
      str_detect(country, "alaska") ~ "united states",
      str_detect(country, "soviet canuckistan") ~ "undisclosed",
      str_detect(country, "n. america") ~ "north america",
      str_detect(country, "singapore") ~ "asia",
      str_detect(country, "taiwan") ~ "asia",
      str_detect(country, "hong kong") ~ "asia",
      str_detect(country, "spain") ~ "europe",
      str_detect(country, "narnia") ~ "undisclosed",
      str_detect(country, "u s a") ~ "united states",
      str_detect(country, "united statea") ~ "united states",
      str_detect(country, "1") ~ "united states",
      str_detect(country, "subscribe to dm4uz3 on youtube") ~ "undisclosed",
      str_detect(country, "usa usa usa!!!!") ~ "united states",
      str_detect(country, "i don't know anymore") ~ "undisclosed",
      str_detect(country, "fear and loathing") ~ "undisclosed",
      TRUE ~ "undisclosed"))

# Item Gifted Column ----


bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(item_gifted = str_remove_all(
    item_gifted, pattern = "[q][0-9][_]"))

unique(bb_candy_all_cleaned$item_gifted)

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(item_gifted = str_replace_all(
    item_gifted, pattern = "_", " "))

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(
    item_gifted = case_when(
      str_detect(item_gifted, "x100 grand bar") ~ "100 grand bar",
      str_detect(item_gifted, "hershey s kissables") ~ "hershey's kissables",
      str_detect(item_gifted, "dark chocolate hershey") ~ "hershey's dark chocolate",
      str_detect(item_gifted, "hershey s milk chocolate") ~ "hershey's milk chocolate",
      str_detect(item_gifted, "jolly rancher bad flavor") ~ "jolly ranchers bad flavor",
      str_detect(item_gifted, "reese s peanut butter cups") ~ "reese's peanut butter cups",
      str_detect(item_gifted, "tolberone something or other") ~ "toblerone",
      str_detect(item_gifted, "peanut m m s") ~ "m&m's peanut",
      str_detect(item_gifted, "regular m ms") ~ "m&m's regular",
      str_detect(item_gifted, "mint m ms") ~ "m&m's mint",
      str_detect(item_gifted, "boxo raisins") ~ "box o raisins",
      str_detect(item_gifted, "hersheys dark chocolate") ~ "hershey's dark chocolate",
      str_detect(item_gifted, "hersheys kisses") ~ "hershey's kisses",
      str_detect(item_gifted, "blue m ms") ~ "m&m's blue",
      str_detect(item_gifted, "red m ms") ~ "m&m's red",
      str_detect(item_gifted, "reeses pieces") ~ "reese's pieces",
      str_detect(item_gifted, "sourpatch kids i e abominations of nature") ~ "sourpatch kids",
      str_detect(item_gifted, "anonymous brown globs that come in black and orange wrappers a k a mary janes") ~ "mary janes",
      str_detect(item_gifted, "green party m ms") ~ "m&m's green",
      str_detect(item_gifted, "independent m ms") ~ "m&m's regular",
      TRUE ~ item_gifted))

unique(bb_candy_all_cleaned$item_gifted)

# Trick or Treat Column ----

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(
    trick_or_treat = recode(trick_or_treat, "yes" = TRUE, "no" = FALSE)
  )

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  mutate(trick_or_treat = as.logical(trick_or_treat))

# Reaction Column ----

unique(bb_candy_all_cleaned$reaction)

bb_candy_all_cleaned <- bb_candy_all_cleaned %>%
  drop_na(reaction)    

# Write Data To Clean Folder For Analysis ----

write_csv(bb_candy_all_cleaned, "clean_data/bb_candy_all_cleaned")

# End ----

