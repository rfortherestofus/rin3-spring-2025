# Load Packages -----------------------------------------------------------

library(tidyverse)
library(gapminder)

# Import Data -------------------------------------------------------------

penguins <- read_csv("data-raw/penguins.csv")

penguins_bill_length_by_island <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

penguins_by_species <-
  penguins |>
  count(species)


# Color vs Fill ----------------------------------------------------------

ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point()

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n
  )
) +
  geom_col()


# Applying values specific color/fill properties -------------------------

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()

# Labels vs breaks -------------------------------------------------------

ggplot(
  penguins_by_species,
  aes(x = species, y = n, fill = species, label = n)
) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  scale_y_continuous(
    limits = c(0, 200),
    labels = c(0, 40, 80, 120, 160)
  ) +
  geom_label(vjust = 1.5, color = "white")

# Dropping Points in Scatterplots ----------------------------------------

ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point() +
  scale_x_continuous(
    limits = c(170, 200)
  )

penguins_filtered <-
  penguins |>
  drop_na(flipper_length_mm, body_mass_g) |>
  filter(flipper_length_mm < 200)

ggplot(
  data = penguins_filtered,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point() +
  scale_x_continuous(
    limits = c(170, 210)
  )

# Bar Chart Width ---------------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    label = mean_bill_length
  )
) +
  geom_col() +
  theme_minimal()

# Reordering Bar Charts ---------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length
  )
) +
  geom_col()

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = reorder(island, mean_bill_length),
    y = mean_bill_length
  )
) +
  geom_col()

penguins_bill_length_by_island_reordered <-
  penguins_bill_length_by_island |>
  mutate(island = fct_reorder(island, mean_bill_length))

ggplot(
  data = penguins_bill_length_by_island_reordered,
  aes(
    x = island,
    y = mean_bill_length
  )
) +
  geom_col()


# Wrapping Long Text ------------------------------------------------------

ggplot(
  data = gapminder,
  aes(
    x = year,
    y = lifeExp,
    group = country
  )
) +
  geom_line() +
  facet_wrap(vars(country))

gapminder_wrapped <-
  gapminder |>
  mutate(country_wrapped = str_wrap(country, width = 10))

ggplot(
  data = gapminder_wrapped,
  aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line() +
  facet_wrap(vars(country_wrapped))
