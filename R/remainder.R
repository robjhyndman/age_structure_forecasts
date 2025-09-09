make_future_Ext_fig <- function(object, data, h, times, yr) {
  object <- object |>
    generate(h = h, times = times) |>
    filter(year == yr, age <= 100)
  if (NROW(object) == 0) {
    title <- "Remainder"
  } else {
    title <- paste("Simulated future remainder:", yr)
  }

  object |>
    ggplot() +
    geom_line(
      data = data |> filter(age <= 100),
      aes(x = age, y = remainder, group = year),
      color = "grey"
    ) +
    geom_line(aes(x = age, y = .sim, col = .rep, group = .rep)) +
    guides(color = "none") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Number of people"), # ($m_{x,t}$)"),
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}

make_future_Ext_fig2 <- function(object, data, h, times, yr, no_other = TRUE) {
  object <- object |>
    forecast(h = h) |>
    filter(year == yr, age <= 100) |>
    mutate(
      pi = hilo(remainder),
      lo = pi$lower,
      hi = pi$upper
    )
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    data <- data |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  if (NROW(object) == 0) {
    title <- "Remainder"
  } else {
    title <- paste("Forecasts of remainder by discipline:", yr)
  }

  data |>
    filter(age <= 100, !is.na(remainder)) |>
    ggplot(aes(x = age)) +
    facet_wrap(~discipline, scales = "free_y") +
    geom_line(
      aes(y = remainder, group = year),
      color = "grey"
    ) +
    geom_ribbon(
      data = object,
      aes(ymin = lo, ymax = hi),
      alpha = 0.3,
      fill = "#c14b14"
    ) +
    geom_line(data = object, aes(y = .mean), color = "#c14b14") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Number of people"), # ($m_{x,t}$)"),
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}
