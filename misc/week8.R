library(tidyverse)

# as.numeric() vs parse_number() -----------------------------------------

names_and_ages <-
  tribble(
    ~name,
    ~age,
    "David",
    "45",
    "Rachel",
    "45",
    "Leila",
    "8",
    "Elias",
    "8 years old (born in 2016)",
    "Diego",
    "No longer alive"
  )

names_and_ages

names_and_ages |>
  mutate(age_v2 = as.numeric(age))

names_and_ages |>
  mutate(age_v2 = parse_number(age))


# case_match() ------------------------------------------------------------

countries <-
  tribble(
    ~country_name,
    "USA",
    "US",
    "United States of America",
    "Canada"
  )

countries |>
  mutate(country_name_v2 = case_match(
    country_name,
    "USA" ~ "USA",
    "US" ~ "USA",
    "United States of America" ~ "USA",
    "Canada" ~ "Canada"
  ))

# case_match() vs case_when() ---------------------------------------------

library(palmerpenguins)

data(penguins)

penguins |>
  select(species) |>
  mutate(
    species_v2 = case_match(
      species,
      "Adelie" ~ "Island 1",
      "Chinstrap" ~ "Island 2",
      "Gentoo" ~ "Island 3"
    )
  ) |>
  view()

penguins |>
  select(species) |>
  mutate(
    species_v2 = case_when(
      species == "Adelie" ~ "Island 1",
      species == "Chinstrap" ~ "Island 2",
      species == "Gentoo" ~ "Island 3"
    )
  ) |>
  view()

penguins |>
  select(species, bill_length_mm) |>
  mutate(
    species_and_length = case_when(
      species == "Adelie" & bill_length_mm > 35 ~ "Big Adelie Penguins",
      .default = "Other"
    )
  ) |>
  view()

# Joins with mismatched variable types ------------------------------------

fruits <-
  tibble(
    id = c(1, 2, 3, 4),
    value = c("apple", "banana", "cherry", "date")
  )

prices <-
  tibble(
    id = c("1", "2", "3", "4"),
    price = c(0.99, 1.50, 2.00, 2.50)
  ) |>
  mutate(id = parse_number(id))

# left_join(
#   fruits,
#   prices,
#   join_by(id)
# )

fruits |>
  left_join(
    prices,
    join_by(id)
  )

# Many-to-many joins ------------------------------------------------------

orders <-
  tibble(
    order_date = c("2024-01-01", "2024-01-01", "2024-01-02"),
    product = c("apple", "apple", "banana"),
    quantity = c(5, 3, 2),
    location = "Store A"
  )

inventory <-
  tibble(
    product = c("apple", "apple", "banana"),
    location = c("Store A", "Store B", "Store A"),
    stock = c(100, 150, 75)
  )

orders |>
  left_join(
    inventory,
    join_by(product, location)
  )

# Iteration --------------------------------------------------------------

# Import multiple years of data

library(readxl)
library(janitor)

total_population_2019 <-
  read_excel(
    "data-raw/2019-obtn-by-county.xlsx",
    sheet = "Total Population"
  ) |>
  clean_names()

total_population_2020 <-
  read_excel(
    "data-raw/2020-obtn-by-county.xlsx",
    sheet = "Total Population"
  ) |>
  clean_names()

bind_rows(
  total_population_2019,
  total_population_2020
)

import_single_year_data <- function(year) {
  read_excel(
    path = str_glue("data-raw/{year}-obtn-by-county.xlsx"),
    sheet = "Total Population"
  ) |>
    clean_names() |>
    mutate(data_year = year)
}

import_single_year_data(year = 2020)

total_population <-
  map(
    2019:2023,
    import_single_year_data
  ) |>
  bind_rows()

# Make multiple plots

make_total_population_plot <- function(county_name) {
  total_population |>
    filter(geography == county_name) |>
    ggplot(
      aes(
        x = data_year,
        y = population
      )
    ) +
    geom_line()

  ggsave(
    filename = str_glue("plots/{county_name}.png")
  )
}

make_total_population_plot("Multnomah")

oregon_counties <-
  total_population |>
  distinct(geography) |>
  pull(geography)

all_plots <- 
map(
  oregon_counties,
  make_total_population_plot
)
