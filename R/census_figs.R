make_pop_fig <- function(
  census,
  subtitle,
  interpolation = FALSE,
  highlight_census = FALSE,
  no_other = TRUE
) {
  census$Year <- census$year
  censuses <- seq(2006, 2021, by = 5)
  if (!interpolation) {
    census <- census |>
      as_tibble() |>
      mutate(Year = factor(Year, levels = rev(censuses)))
  }
  if (no_other & "discipline" %in% names(census)) {
    census <- census |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  if ("discipline" %in% names(census)) {
    census$discipline <- factor(
      census$discipline,
      levels = c(
        "Biological Sciences",
        "Chemical Sciences",
        "Earth Sciences",
        "Mathematical Sciences",
        "Physics and Astronomy",
        "Other Natural and Physical Sciences"
      )
    )
  }

  p <- census |>
    as_tibble() |>
    ggplot() +
    aes(x = age, y = working, color = Year, group = Year) +
    geom_line(
      data = census |> filter(Year %in% censuses),
      linewidth = 0.4 + 0.2 * highlight_census
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    labs(
      x = "Age",
      y = "Number of active scientists",
      title = "Working population by discipline",
    )
  if ("discipline" %in% names(census)) {
    p <- p +
      facet_wrap(~discipline, scales = "free_y") +
      theme(
        legend.direction = if_else(!interpolation, "vertical", "horizontal"),
        legend.position = "inside",
        legend.position.inside = c(2 / 3, 1 / 2),
        legend.justification = c(
          0.6 * interpolation - 1.0,
          1.6 * interpolation + 1.4
        )
      )
  }
  if (interpolation) {
    p <- p +
      geom_line(
        data = census |> filter(!Year %in% censuses),
        linewidth = 0.4,
        alpha = 0.7
      ) +
      scale_color_gradientn(colours = rainbow(1000)[1:800])
  } else {
    cols <- c("#ff0000", "#67ff00", "#00ceff", "#ca00ff")
    names(cols) <- seq(2006, 2021, by = 5)
    p <- p +
      scale_color_manual(values = cols)
  }
  p
}
