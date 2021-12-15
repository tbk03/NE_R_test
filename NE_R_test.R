library(tidyverse)
library(readxl)

data_in <- readxl::read_xlsx("NE_R_Test.xlsx")

#' Counts the number of recipes ingredients appear in
#'
#' @param df_data: a data frame with id, dish, area, ingredients columns.
#' the ingredients columns should contain a string of comma separated ingredients

#' @return: a dataframe with the top 3 ingredients (ordered appropriately)

count_recipes_ingredients_appear_in <- function(df_data){

  # define arbitrary column names for separating ingredients into
  # number of columns set to ensure there are no data lost from ingredient strings
  # (number of columns identified through experimentation and reading warning messages)
  max_num_ingredients <- 100
  colum_names <- as.character(1:max_num_ingredients)

  # process data
  df_res <- df_data %>%

    # identify individual ingredients in each recipe
    separate(ingredients, sep = ",", into = colum_names) %>%

    # move to long format
    pivot_longer(cols = c(`1`:as.character(max_num_ingredients)),
                 values_to = "ingredient") %>%
    na.omit() %>% # remove NAs created by pivoting

    # simplify dataframe by dropping columns that aren't needed
    select(-c(dish, area, name)) %>%

    # make sure all ingredients are in lower case
    # and remove trailing white space
    # so we get the correct counts
    mutate(ingredient = str_to_lower(ingredient),
           ingredient = str_trim(ingredient)) %>%

    # count number of times ingredients appear across the recipes
    group_by(ingredient) %>%
    summarise(count = n()) %>%
    ungroup() %>%

    # identify top three ingredients and present them in a logical order
    slice_max(count, n = 3) %>%
    arrange(desc(count), ingredient)

  # return results
  return(df_res)

}

count_recipes_ingredients_appear_in(data_in)
