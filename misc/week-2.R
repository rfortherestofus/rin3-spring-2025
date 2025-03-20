library(tidyverse)

# Working directories -----------------------------------------------------

penguins <- read_csv("data-raw/penguins_data.csv")

penguins_mean_bill_length <- penguins |> 
  filter(island == "Biscoe") |> 
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

# Native pipe vs tidyverse pipe -------------------------------------------


# Parentheses -------------------------------------------------------------

penguins |>
  select(-(bill_length_mm:body_mass_g))


# select() issues ---------------------------------------------------------

penguins |>
  select(-island:year)

penguins |>
  select(-1, island:year)

# Does not remove the "species" variable but this does:

penguins |>
  select(island:year)


# NA values ---------------------------------------------------------------

read_csv("data-raw/penguins_data.csv")


# NA values ---------------------------------------------------------------

penguins <- read_csv(file = "data-raw/penguins.csv")

penguins |>
  mutate(not_actually_na = "NA") |>
  mutate(actually_na = na_if(not_actually_na, "NA")) |>
  mutate(really_not_na = replace_na(actually_na, "NA"))


# Rounding ----------------------------------------------------------------

penguins |>
  filter(island == "Biscoe") |>
  drop_na(body_mass_g, sex) |>
  group_by(sex) |>
  summarize(mean_body_mass = mean(body_mass_g)) |>
  mutate(mean_body_mass = round(mean_body_mass, digits = 0))

# Viewing your dataset ----------------------------------------------------

penguins |>
  select(species, island)

penguins |>
  print(n = 100)


