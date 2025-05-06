make_pop_fig <- function(
  census,
  subtitle,
  interpolation = FALSE,
  highlight_census = FALSE,
  no_other = TRUE
) {
  if (!interpolation) {
    census <- census |>
      as_tibble() |>
      mutate(year = factor(year, levels = rev(seq(2006, 2021, by = 5))))
  }
  if (no_other & "discipline" %in% names(census)) {
    census <- census |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
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
      title = paste("Working population:", subtitle, "(2006 – 2021)")
    )
  if ("discipline" %in% names(census)) {
    p <- p + facet_wrap(~discipline, scales = "free_y")
  }
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
  if (var == "remainder") {
    ylab = "Remainder"
  } else {
    ylab = paste("Estimated number of", var)
  }
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
      y = ylab,
      title = paste0(
        ylab,
        ": Natural and Physical Sciences (",
        minyear,
        " – ",
        maxyear,
        ")"
      )
    )
}

make_fig1 <- function(census_science) {
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF", "#000000")

  census_science |>
    filter(year %in% c(2016, 2021)) |>
    ggplot() +
    aes(x = age, y = participation, color = as.factor(year)) +
    geom_point(size = 1.5) +
    geom_line(aes(y = .smooth), color = "black") +
    labs(
      title = "Labour Force Participation: Natural and Physical Sciences",
      x = "Age",
      y = "Participation rate",
      shape = "Census Year",
      color = "Census Year"
    ) +
    scale_x_continuous(
      breaks = seq(20, 100, by = 10),
      labels = c(seq(20, 90, by = 10), "100+")
    ) +
    scale_shape_manual(values = c(1, 2, 0)) +
    scale_color_manual(values = cols)
}

make_pop_future_fig <- function(object, data, yrs, ribbon = FALSE) {
  lapply(
    yrs,
    make_pop_future_fig_year,
    object = object,
    data = data,
    ribbon = ribbon
  )
}

make_pop_future_fig_year <- function(yr, object, data, ribbon = FALSE) {
  object <- object |> filter(year == yr)
  if (!ribbon) {
    object <- object |>
      filter(stringr::str_length(.rep) < 2)
  }
  if (NROW(object) == 0) {
    title <- "Working population"
  } else if (!ribbon) {
    title <- paste("Simulated future working population:", yr)
  } else {
    title <- paste("90% forecast interval for working population:", yr)
  }
  p <- data |>
    as_tibble() |>
    ggplot() +
    aes(x = age, y = working) +
    geom_line(aes(group = year), color = "gray") +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    labs(
      x = "Age",
      y = "Number of active scientists",
      title = title
    )
  if (!ribbon) {
    p <- p +
      geom_line(
        data = object,
        aes(x = age, y = working, group = .rep, col = .rep)
      )
  } else if (NROW(object) > 0) {
    object <- object |>
      as_tibble() |>
      group_by(age) |>
      summarise(
        .lower = quantile(working, probs = 0.1),
        .upper = quantile(working, probs = 0.9),
        working = mean(working),
      )
    p <- p +
      geom_ribbon(
        data = object,
        aes(ymin = .lower, ymax = .upper),
        alpha = 0.3,
        fill = "#c14b14"
      ) +
      geom_line(data = object, color = "#c14b14", linewidth = 1)
  }
  p +
    guides(color = "none")
}
