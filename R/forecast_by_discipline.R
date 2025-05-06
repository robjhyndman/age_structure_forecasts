get_ymax <- function(
  yrs,
  object,
  no_other = FALSE
) {
  # Remove other group
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  object <- object |>
    as_tibble() |>
    dplyr::filter(year %in% yrs) |>
    group_by(age, discipline, year) |>
    summarise(
      .upper = quantile(working, probs = 0.9),
      .groups = "drop"
    ) |>
    group_by(discipline) |>
    summarise(ymax = max(.upper))
}

make_pop_future_fig_discipline <- function(
  yrs,
  object,
  data,
  no_other = FALSE,
  ymax,
  color_data = FALSE
) {
  # Remove other group
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    data <- data |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  object <- object |>
    as_tibble() |>
    dplyr::filter(year %in% yrs) |>
    group_by(age, discipline, year) |>
    summarise(
      .lower = quantile(working, probs = 0.1),
      .upper = quantile(working, probs = 0.9),
      working = mean(working),
      .groups = "drop"
    )
  lapply(
    yrs,
    make_pop_future_fig_discipline_year,
    object = object,
    data = data,
    ymax = ymax,
    color_data = color_data
  )
}

make_pop_future_fig_discipline_year <- function(
  yrs,
  object,
  data,
  ymax,
  color_data
) {
  disciplines <- unique(data$discipline)
  object <- object |>
    dplyr::filter(year %in% yrs)
  p <- lapply(disciplines, function(d) {
    tmp <- data |>
      filter(discipline == d) |>
      ggplot() +
      aes(x = age, y = working, group = factor(year)) +
      facet_wrap(~discipline, scales = "free_y")
    if (color_data) {
      tmp <- tmp +
        geom_line(aes(color = year)) +
        scale_color_gradientn(colors = rainbow(10))
    } else {
      tmp <- tmp + geom_line(color = "gray")
    }
    if (NROW(object) > 0) {
      tmp <- tmp +
        geom_ribbon(
          data = object |> filter(discipline == d),
          aes(ymin = .lower, ymax = .upper, fill = factor(year)),
          alpha = 0.3
        ) +
        geom_line(
          data = object |> filter(discipline == d),
          aes(color = factor(year)),
          linewidth = 0.75
        ) +
        guides(color = "none")
    }
    tmp +
      scale_x_continuous(breaks = seq(20, 100, by = 10)) +
      labs(
        x = "Age",
        y = "Number of active scientists",
        fill = "Forecast year"
      ) +
      ylim(0, ymax |> filter(discipline == d) |> pull(ymax))
  })
  p[[length(p) + 1]] <- patchwork::guide_area()
  patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "90% forecast interval for working population by discipline",
    ) +
    patchwork::plot_layout(guides = "collect", axis_titles = "collect")
}

make_fig_future_grads_discipline <- function(course_leavers, future_grads_discipline, no_other = TRUE) {
  if (no_other) {
    course_leavers <- course_leavers |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    future_grads_discipline <- future_grads_discipline |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  future_grads_discipline_pi <- future_grads_discipline |>
      as_tibble() |>
      group_by(discipline, year) |>
      summarise(
        lo = quantile(graduates, prob = 0.05),
        mean = mean(graduates),
        hi = quantile(graduates, prob = 0.95),
        .groups = "drop"
      )
  course_leavers |>
    ggplot(aes(x = year)) +
    facet_wrap(~discipline, scales = "free_y") +
    geom_line(aes(y = graduates)) +
    geom_ribbon(
      data = future_grads_discipline_pi,
      aes(x = year, ymin = lo, ymax = hi),
      alpha = 0.3,
      fill = "#c14b14"
    ) +
    labs(
      x = "Year",
      y = "Number of graduates",
      title = "Graduates by discipline"
    ) +
    guides(color = "none")
}
