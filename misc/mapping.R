library(tidyverse)
library(sf)
library(janitor)

# Portland ----------------------------------------------------------------

portland_boundaries <-
  read_sf("data-raw/City_Boundaries.geojson") |>
  clean_names() |>
  filter(cityname == "Portland")

traffic_signals <-
  read_sf("data-raw/Traffic_Signals.geojson") |>
  clean_names()

snow_and_ice_routes <-
  read_sf("data-raw/Snow_and_Ice_Routes.geojson") |>
  clean_names()

# Tigris ------------------------------------------------------------------

library(tigris)

us_states <- states()

us_states

oregon_counties <- counties(state = "Oregon")

# Median Income -----------------------------------------------------------

library(tidycensus)
library(scales)

median_income <-
  get_acs(
    state = "Oregon",
    geography = "county",
    variables = "B19013_001",
    geometry = TRUE
  )

median_income |>
  ggplot(aes(fill = estimate)) +
  geom_sf()


# International Data ------------------------------------------------------

library(rnaturalearth)

iceland <-
  ne_countries(
    country = "Iceland",
    scale = "large",
    returnclass = "sf"
  ) |>
  select(sovereignt)

ggplot(data = iceland) +
  geom_sf()


# Interactive -------------------------------------------------------------

library(ggiraph)

median_income_interactive_plot <-
  median_income |>
  ggplot(aes(
    fill = estimate,
    tooltip = NAME
  )) +
  geom_sf_interactive()

girafe(ggobj = median_income_interactive_plot)
