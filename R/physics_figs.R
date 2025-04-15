make_fig13 <- function(physics, subtitle) {
  physics |>
    ggplot(aes(x = Year, y = Graduates)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
    labs(
      title = "Graduates",
      subtitle = paste0(subtitle, "\n2006–2023"),
      y = "Number of graduates"
    )
}

make_fig14 <- function(future_physics_grads, physics_grads) {
  future_physics_grads <- future_physics_grads |>
    summarise(
      mean = mean(Graduates),
      lo = quantile(Graduates, prob = 0.05),
      hi = quantile(Graduates, prob = 0.95)
    )
  ggplot(future_physics_grads) +
    aes(x = Year) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = physics_grads, aes(y = Graduates)) +
    labs(
      y = "Number of graduates",
      title = "Forecast of Graduates",
      subtitle = "Physics and Astronomy\nForecasted Years: 2024–2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5))
}

make_fig15 <- function(physics) {
  physics |>
    autoplot(Migrants) +
    scale_x_continuous(breaks = seq(10, 100, by = 10)) +
    labs(
      y = "Number of net migrants",
      title = "Estimated Net Migration",
      subtitle = "Physics and Astronomy\n2006 - 2020"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
}


make_fig16 <- function(physics, future_physics) {
  p <- physics |>
    ggplot() +
    aes(x = Age, y = Working, group = Year) +
    geom_line(colour = "gray")
  p1 <- p +
    geom_line(
      data = future_physics |> filter(.rep == "1"),
      aes(colour = Year)
    ) +
    labs(y = "Number of working scientists") +
    scale_color_gradientn(colors = rainbow(10)[1:8]) +
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
      aes(colour = Year)
    ) +
    labs(y = NULL) + # Remove y-axis label
    scale_color_gradientn(colors = rainbow(10)[1:8]) +
    labs(
      title = "Simulation 2",
      subtitle = "Forecasted Years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10))

  p1 + p2
}

make_fig17 <- function(future_physics) {
  # 2025 and 2035
  eg_forecast <- future_physics |>
    filter(Year %in% c(2025, 2035)) |>
    group_by(Age) |>
    summarise(
      mean = mean(Working),
      lo = quantile(Working, prob = 0.05),
      hi = quantile(Working, prob = 0.95),
      .groups = "drop"
    )

  # Overlay
  ggplot(eg_forecast) +
    aes(x = Age, color = factor(Year)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = factor(Year)), alpha = 0.1) +
    geom_line(aes(y = mean), linewidth = 0.75) +
    labs(
      y = "Number of working scientists",
      title = "Forecast of Working Population",
      subtitle = "Physics and Astronomy",
      color = "Forecast Year",
      fill = "Forecast Year"
    ) +
    scale_color_manual(values = c("2025" = "#66666644", "2035" = "#c14b14")) +
    scale_fill_manual(values = c("2025" = "#66666644", "2035" = "#c14b1444")) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    theme(
      legend.position = "top"
    )
}


make_fig18 <- function(physics, future_physics) {
  sum_physics <- physics |>
    as_tibble() |>
    group_by(Year) |>
    summarise(Working = sum(Working), .groups = "drop")
  sum_future_physics <- future_physics |>
    as_tibble() |>
    group_by(Year, .rep) |>
    summarise(Working = sum(Working), .groups = "drop") |>
    group_by(Year) |>
    summarise(
      mean = mean(Working),
      lo = quantile(Working, prob = 0.05),
      hi = quantile(Working, prob = 0.95),
      .groups = "drop"
    )
  ggplot(sum_future_physics) +
    aes(x = Year) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = sum_physics, aes(y = Working)) +
    labs(y = "Total number of working scientists") +
    labs(
      title = "Forecast of Total Working Population",
      subtitle = "Physics and Astronomy\nForecasted Years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    scale_y_continuous(labels = scales::comma_format())
}
