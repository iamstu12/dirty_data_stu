

# Task 4 - 2015 Data Clean

# Load Data and Libraries ----

bb_candy_2015 <- read_excel("/Users/Natifu/dirty_data_stu/task_4_halloween/raw_data/boing-boing-candy-2015.xlsx")

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(here)
library(dplyr)
library(assertr)

# Convert All Data to Lowercase ----

bb_candy_2015_cleaned <- bb_candy_2015 %>%
  mutate_all(str_to_lower) %>%
  clean_names()

# Select Relevant Columns ----

bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
  select(-please_leave_any_remarks_or_comments_regarding_your_choices,
         -please_list_any_items_not_included_above_that_give_you_joy,
         -please_list_any_items_not_included_above_that_give_you_despair,
         -guess_the_number_of_mints_in_my_hand,
         -betty_or_veronica,
         -check_all_that_apply_i_cried_tears_of_sadness_at_the_end_of,
         -that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was,
         -fill_in_the_blank_taylor_swift_is_a_force_for,
         -what_is_your_favourite_font,
         -if_you_squint_really_hard_the_words_intelligent_design_would_look_like,
         -fill_in_the_blank_imitation_is_a_form_of,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jk_rowling,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jj_abrams,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_bieber,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_kevin_bacon,
         -please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
         -which_day_do_you_prefer_friday_or_sunday,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_bruce_lee,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jk_rowling,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_malala_yousafzai,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_thom_yorke,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jj_abrams,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_hillary_clinton,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_donald_trump,
         -please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_beyonce_knowles)

colnames(bb_candy_2015_cleaned)

# Pivot Data Set to Long Format ----

bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
  pivot_longer(
    cols = "butterfinger" : "necco_wafers",
    names_to = "item_gifted",
    values_to = "reaction"
  )

# Rename Columns ----

bb_candy_2015_cleaned <- bb_candy_2015_cleaned %>%
  rename(
    date = timestamp,
    age = how_old_are_you,
    trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself
  )

# Write Data To Clean Folder For Later Use ----

write_csv(bb_candy_2015_cleaned, "clean_data/bb_candy_2015_cleaned")

