library(tidyverse)
library(janitor)
library(sf)

# Portland ----------------------------------------------------------------

portland_boundaries <-
  read_sf("data-raw/City_Boundaries.geojson") |>
  clean_names() |>
  filter(cityname == "Portland")

portland_boundaries |> 
  ggplot() +
  geom_sf()

traffic_signals <-
  read_sf("data-raw/Traffic_Signals.geojson") |>
  clean_names()

traffic_signals |> 
  ggplot() +
  geom_sf()

snow_and_ice_routes <-
  read_sf("data-raw/Snow_and_Ice_Routes.geojson") |>
  clean_names()

snow_and_ice_routes |> 
  ggplot() +
  geom_sf()

ggplot() +
  geom_sf(data = portland_boundaries) +
  geom_sf(data = traffic_signals,
          aes(color = software_type),
          alpha = 0.5,
          size = 1) +
  geom_sf(data = snow_and_ice_routes) +
  theme_dk(show_axis_text = FALSE)

# Tigris ------------------------------------------------------------------

library(tigris)

us_states <- states()

us_states |> 
  shift_geometry() |> 
  ggplot() +
  geom_sf()

kentucky_counties <- counties(state = "Kentucky")

kentucky_counties |> 
  ggplot() +
  geom_sf()

# Median Income -----------------------------------------------------------

library(tidycensus)
library(scales)

median_income <-
  get_acs(
    state = "Washington",
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
    tooltip = estimate
  )) +
  geom_sf_interactive()

girafe(ggobj = median_income_interactive_plot)
