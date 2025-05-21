library(tidyverse)
library(ggtext)
library(palmerpenguins)


theme_dk <- function() {
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.title = element_markdown(),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      axis.text = element_text(color = "grey60", size = 10)
    )
}

penguin_bar_chart <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE)) |>
  ggplot(
    aes(
      x = island,
      y = mean_bill_length,
      label = island,
      fill = island
    )
  ) +
  geom_col()

penguin_bar_chart

penguin_bar_chart +
  theme_dk()
