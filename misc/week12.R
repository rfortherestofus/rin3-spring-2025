
# Functions that take variables as arguments ------------------------------

library(tidyverse)

penguins <- read_csv("data-raw/penguins.csv")

filter_by_year <- function(year_to_filter) {
  penguins |> 
    filter(year == year_to_filter)
}

filter_by_year(year_to_filter = 2008)

calculate_mean <- function(variable) {
  penguins |> 
    summarize(avg = mean(variable, na.rm = TRUE))
}

calculate_mean(bill_length_mm)
