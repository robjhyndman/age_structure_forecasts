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

make_fig19 <- function(disciplines_combined) {
  years <- seq(2006, 2021, by = 5)
  cols <- c("#ff0000", "#67ff00", "#00ceff", "#ca00ff")
  names(cols) <- years

  disciplines_combined |>
    as_tibble() |>
    filter(year %in% years) |>
    mutate(Year = factor(year, levels = rev(years))) |>
    ggplot() +
    aes(x = age, y = working, color = Year, group = Year) +
    geom_line() +
    scale_color_manual(values = cols) +
    labs(
      y = "Number of active scientists",
      title = "Working population by discipline",
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    facet_wrap(~discipline, scales = "free_y") +
    theme(
      legend.position = "right"
    )
}

make_fig20 <- function(disciplines_combined) {
  ggplot(disciplines_combined) +
    aes(x = age, y = working, group = year, color = year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Number of active scientists",
      title = "Interpolated working population by discipline",
      subtitle = "Natural and Physical Sciences\n2006 - 2021"
    ) +
    theme(
      legend.position = "top"
    )
}

make_fig21 <- function(course_leavers, combine = FALSE, no_other = TRUE) {
  if (no_other) {
    course_leavers <- course_leavers |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  course_leavers$discipline <- factor(
    course_leavers$discipline,
    levels = c(
      "Biological Sciences",
      "Chemical Sciences",
      "Earth Sciences",
      "Mathematical Sciences",
      "Physics and Astronomy",
      "Other Natural and Physical Sciences"
    )
  )
  subtitle <- "(2006 - 2023)"
  if (combine) {
    course_leavers <- course_leavers |>
      group_by(year) |>
      summarise(graduates = sum(graduates), .groups = "drop")
  }

  p <- ggplot(course_leavers) +
    aes(x = year, y = graduates) +
    geom_line() +
    labs(
      x = "Year",
      y = "Number of graduates",
    ) +
    theme(legend.position = "none")

  if (!combine) {
    p <- p +
      facet_wrap(~discipline, scales = "free_y") +
      labs(title = "Graduates by discipline")
  } else {
    p <- p +
      labs(title = paste("Total graduates:", subtitle))
  }
  p
}

make_fig22 <- function(disciplines_combined, no_other = TRUE) {
  disciplines_combined$discipline <- factor(
    disciplines_combined$discipline,
    levels = c(
      "Biological Sciences",
      "Chemical Sciences",
      "Earth Sciences",
      "Mathematical Sciences",
      "Physics and Astronomy",
      "Other Natural and Physical Sciences"
    )
  )
  if (no_other) {
    disciplines_combined <- disciplines_combined |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  disciplines_combined$Year <- disciplines_combined$year
  disciplines_combined$Age <- disciplines_combined$age
  ggplot(disciplines_combined) +
    aes(x = Age, y = remainder, group = Year, color = Year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(10, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Remainder",
      title = "Remainder by discipline"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    theme(
      legend.direction = "horizontal",
      legend.position = "inside",
      legend.position.inside = c(2 / 3, 1 / 2),
      legend.justification = c(-0.4, 3)
    )
}
