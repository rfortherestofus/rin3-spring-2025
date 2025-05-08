#  Recreating this: https://show.rfor.us/cditiy

library(tidyverse)
library(ggchicklet)
library(scales)
library(patchwork)

cbem <- read_csv("https://rin3spring2025.rfortherestofus.com/data-raw/cbem.csv")

cbem_plot <- function(location_to_filter, age_group_to_filter) {
  
  max_y_value <- 
    cbem |>
    filter(location == location_to_filter) |>
    filter(group != "All Persons") |> 
    slice_max(order_by = percent,
              n = 1) |> 
    pull(percent)
  
  
  cbem_state_average <-
    cbem |>
    filter(location == location_to_filter) |>
    filter(age_group == age_group_to_filter) |>
    filter(group == "All Persons") |>
    pull(percent)

  cbem_state_average_text <-
    str_glue("CBEM State Rate\n{percent(cbem_state_average, accuracy = 0.1)}")


  cbem_single_state_and_age <-
    cbem |>
    filter(location == location_to_filter) |>
    filter(age_group == age_group_to_filter) |>
    filter(group != "All Persons") |>
    mutate(group = fct(
      group,
      levels = c(
        "American Indian or Alaska Native",
        "Asian or Pacific Islander",
        "Black or African American",
        "White",
        "Hispanic or Latino"
      )
    )) |>
    mutate(x_position = case_when(
      group == "American Indian or Alaska Native" ~ 1,
      group == "Asian or Pacific Islander" ~ 2,
      group == "Black or African American" ~ 3,
      group == "White" ~ 4,
      group == "Hispanic or Latino" ~ 5.5
    )) |>
    mutate(percent_formatted = percent(percent))

  cbem_single_state_and_age |>
    ggplot(
      aes(
        x = x_position,
        y = percent,
        fill = group,
        label = percent_formatted
      )
    ) +
    geom_hline(
      yintercept = cbem_state_average,
      linetype = "dashed",
      color = "grey40"
    ) +
    geom_chicklet() +
    geom_text(
      vjust = 1.5,
      color = "white",
      family = "Montserrat",
      size = 5
    ) +
    annotate(
      geom = "text",
      x = 5.5,
      y = cbem_state_average,
      vjust = -0.8,
      color = "grey40",
      family = "Montserrat",
      label = cbem_state_average_text
    ) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#9CC892",
        "Asian or Pacific Islander" = "#0066cc",
        "Black or African American" = "#477A3E",
        "White" = "#6CC5E9",
        "Hispanic or Latino" = "#ff7400"
      )
    ) +
    scale_y_continuous(
      limits = c(0, max_y_value)
    ) +
    theme_void() +
    theme(legend.position = "none")
}



cbem_plot_combined <- function(state) {
  under_18_plot <-
    cbem_plot(
      location_to_filter = state,
      age_group_to_filter = "Under 18"
    )
  
  under_25_plot <-
    cbem_plot(
      location_to_filter = state,
      age_group_to_filter = "Under 25"
    )
  
  under_18_plot + under_25_plot
}

cbem_plot_combined(state = "New York")

