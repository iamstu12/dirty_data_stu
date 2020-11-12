

# Task 4 - 2016 Data Clean

# Load Data and Libraries ----

bb_candy_2016 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2016.xlsx")

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(here)
library(dplyr)
library(assertr)

# Convert All Data to Lowercase ----

bb_candy_2016_cleaned <- bb_candy_2016 %>%
  mutate_all(str_to_lower) %>%
  clean_names()

# Select All Relevant Columns ----

colnames(bb_candy_2016_cleaned)

bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
  select(-which_state_province_county_do_you_live_in,
         -person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
         -please_list_any_items_not_included_above_that_give_you_joy,
         -please_list_any_items_not_included_above_that_give_you_despair,
         -please_leave_any_witty_snarky_or_thoughtful_remarks_or_comments_regarding_your_choices,
         -guess_the_number_of_mints_in_my_hand,
         -betty_or_veronica,
         -that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was,
         -what_is_your_favourite_font,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jk_rowling,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jj_abrams,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_bieber,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_kevin_bacon,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
         -which_day_do_you_prefer_friday_or_sunday,
         -do_you_eat_apples_the_correct_way_east_to_west_side_to_side_or_do_you_eat_them_like_a_freak_of_nature_south_to_north_bottom_to_top,
         -when_you_see_the_above_image_of_the_4_different_websites_which_one_would_you_most_likely_check_out_please_be_honest,
         -york_peppermint_patties_ignore)

colnames(bb_candy_2016_cleaned)

# Pivot Data Set to Long Format ----

bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
  pivot_longer(
    cols = "x100_grand_bar" : "york_peppermint_patties",
    names_to = "item_gifted",
    values_to = "reaction"
  )

bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
  select(-york_peppermint_patties_ignore)

# Rename Columns ----

bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
  rename(
    date = timestamp,
    age = how_old_are_you,
    trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in
  )

bb_candy_2016_cleaned <- bb_candy_2016_cleaned %>%
  rename(
    country = which_country_do_you_live_in
)

colnames(bb_candy_2016_cleaned)

# Write Data To Clean Folder For Later Use ----

write_csv(bb_candy_2016_cleaned, "clean_data/bb_candy_2016_cleaned")

# End ----
         
         
         
         





