
# Functions as Recipes ----------------------------------------------------

# https://joyfoodsunshine.com/the-most-amazing-chocolate-chip-cookies/#wprm-recipe-container-8678

# Show in Excel ----------------------------------------------------------

library(tidyverse)
library(fs)

penguins <- read_csv("https://data.rfortherestofus.com/penguins-2007.csv")

show_in_excel_penguins <- function() {
  csv_file <- "my-data.csv"

  write_csv(
    x = penguins,
    file = csv_file,
    na = ""
  )

  file_show(path = csv_file)
}

show_in_excel_penguins()

show_in_excel <- function(data) {
  csv_file <- "my-data.csv"
  
  write_csv(
    x = data,
    file = csv_file,
    na = ""
  )
  
  file_show(path = csv_file)
}

show_in_excel(data = palmerpenguins::penguins)

# ACS Data ---------------------------------------------------------------

library(tidycensus)
library(janitor)

race_ethnicity_data <-
  get_acs(
    geography = "state",
    variables = c(
      "B03002_003",
      "B03002_004",
      "B03002_005",
      "B03002_006",
      "B03002_007",
      "B03002_008",
      "B03002_009",
      "B03002_012"
    )
  )

# Basic function

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "B03002_003",
        "B03002_004",
        "B03002_005",
        "B03002_006",
        "B03002_007",
        "B03002_008",
        "B03002_009",
        "B03002_012"
      )
    )

  race_ethnicity_data
}

# Change variable value text

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )

  race_ethnicity_data
}

get_acs_race_ethnicity()

# Add argument

get_acs_race_ethnicity <- function(clean_variable_names = FALSE) {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )

  if (clean_variable_names == TRUE) {
    race_ethnicity_data <- clean_names(race_ethnicity_data)
  }

  race_ethnicity_data
}

get_acs_race_ethnicity(clean_variable_names = TRUE)

# ...

get_acs_race_ethnicity <- function(
  clean_variable_names = FALSE,
  ...
) {
  race_ethnicity_data <-
    get_acs(
      ...,
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )

  if (clean_variable_names == TRUE) {
    race_ethnicity_data <- clean_names(race_ethnicity_data)
  }

  race_ethnicity_data
}

get_acs_race_ethnicity(
  geography = "county",
  clean_variable_names = TRUE
)
