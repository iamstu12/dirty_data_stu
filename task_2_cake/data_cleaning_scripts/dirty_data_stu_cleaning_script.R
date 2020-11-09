

# 1.2 Task 2 - Cake Ingredients ----

# Load Libraries ----

library(tidyverse)
library(janitor)
library(here)

# Read in Data ----

# Check where the top level of the project directory is using 'here'

here::here()

# Use this link to set the path to the data file

cake_names <- read_csv(here("raw_data/cake-ingredients-1961.csv"))

cake_ingredients <- read_csv(here("raw_data/cake_ingredient_code.csv"))

# 1.2.1 Cleaning The Data ----

# STAGE ONE - CONVERT TO LONG FORMAT

cake_long_format <- cake_names %>%
  pivot_longer( #1
    cols = "AE" : "ZH", #2
    names_to = "ingredient_code", #3
    values_to = "measurement" #4
  )

#1 - Step 1 is to adjust the table into long format
#2 - Step 2 is to specify what columns are to be converted to long
#3 - Step 3 is to name the new column where the old columns are to go into
#4 - Step 4 moves the values of the old column into a new column

# STAGE TWO - REMOVE MISSING VALUES AND CLEAN NAMES

# As you will see from the results, there are a large number of 'nas', which
# make the table messy. As we do not need the 'nas', I would drop them, followed,
# by a cleaning the names to ensure all column names are lower case.

cake_long_format <- cake_names %>%
  pivot_longer(
    cols = "AE" : "ZH",
    names_to = "ingredient_code",
    values_to = "measurement"
  ) %>%
  drop_na() %>%
  clean_names()

# STAGE THREE - JOIN TABLES TO INCLUDE INGREDIENT NAME

# The first step is to change the name of the 'code' column to
# 'ingredient_code' so that the two tables have the same column name

cake_ingredients_adjusted <- rename(cake_ingredients, ingredient_code = code)

# The next step is to join the tables

cake_long_format_joined <- cake_long_format %>%
  left_join(cake_ingredients_adjusted, by = "ingredient_code") %>% #1
  select(cake, measurement, measure, ingredient) #2

#1 - Step 1, I have decided to use a left join in order to keep the same 
    # columns from the 'cake_long_format' table and I have connected the
    # two tables using the 'ingredient_code' column.
#2 - Step 2 is to remove the 'ingredient_code' column from view in order
    # to only show the ingredient's actual name.

# STAGE FOUR - CHANGE THE 'ONE' MEASUREMENT TO 'WHOLE'

# As you will see from the results of the previous table, it reads in a 
# complicated way - '10 one Egg White', so I changed 'one' to 'whole',
# which reads better.

cake_long_format_cleaned <- cake_long_format_joined %>%
  mutate(
    measure = recode(measure, "one" = "whole"))














