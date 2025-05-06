make_fig16 <- function(physics, future_physics) {
  p <- physics |>
    ggplot() +
    aes(x = age, y = working, group = year) +
    geom_line(colour = "gray")
  p1 <- p +
    geom_line(
      data = future_physics |> filter(.rep == "1"),
      aes(colour = year)
    ) +
    labs(y = "Number of working scientists") +
    scale_color_gradientn(colors = rainbow(10)) +
    labs(
      title = "Simulation 1",
      subtitle = "Forecasted Years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    theme(
      legend.position = "none" # Remove legend
    )

  p2 <- p +
    geom_line(
      data = future_physics |> filter(.rep == "2"),
      aes(colour = year)
    ) +
    labs(y = NULL) + # Remove y-axis label
    scale_color_gradientn(colors = rainbow(10)) +
    labs(
      title = "Simulation 2",
      subtitle = "Forecasted Years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10))

  p1 + p2
}
