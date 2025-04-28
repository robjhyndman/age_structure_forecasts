make_pop_fig <- function(
  census,
  subtitle,
  interpolation = FALSE,
  highlight_census = FALSE
) {
  if(!interpolation) {
    census <- census |>
      as_tibble() |>
      mutate(year = factor(year, levels = rev(seq(2006, 2021, by = 5))))
  }
  p <- census |>
    as_tibble() |>
    ggplot() +
    aes(x = age, y = working, color = year, group = year) +
    geom_line(
      data = census |> filter(year %in% seq(2006, 2021, by = 5)),
      linewidth = 0.5 + 0.5 * highlight_census
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    labs(
      x = "Age",
      y = "Number of active scientists",
      title = paste("Working Population:", subtitle, "(2006 – 2021)")
    )
  if (interpolation) {
    p <- p +
      geom_line(
        data = census |> filter(!year %in% seq(2006, 2021, by = 5)),
        linewidth = 0.5
      ) +
        scale_color_gradientn(colours = rainbow(10))
  } else {
    cols <- c("#ff0000", "#32ff00", "#0065ff", "#ff0099")
    names(cols) <- seq(2006, 2021, by = 5)
    p <- p +
      scale_color_manual(values = cols)
  }
  p
}

make_component_fig <- function(census, variable) {
  var <- as.character(substitute(variable))
  minyear <- min(census$year)
  maxyear <- max(census$year)
  census |>
    as_tibble() |>
    ggplot() +
    aes(x = age, y = {{ variable }}, color = year, group = year) +
    geom_line() +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_color_gradientn(colours = rainbow(10)) +
    labs(
      x = "Age",
      y = paste("Estimated number of", var),
      title = paste0(
        "Natural and Physical Sciences (",
        minyear,
        " – ",
        maxyear,
        ")"
      )
    )
}
