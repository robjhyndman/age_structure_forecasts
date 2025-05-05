make_pop_future_fig_discipline <- function(
    yrs,
    object,
    data,
    no_other = FALSE) {
  lapply(yrs, make_pop_future_fig_discipline_year, object = object, data = data, no_other = no_other)
}

make_pop_future_fig_discipline_year <- function(
    yrs,
    object,
    data,
    no_other = FALSE) {
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
  disciplines <- unique(data$discipline)
  p <- lapply(disciplines, function(d) {
    data |>
      filter(discipline == d) |>
      ggplot() +
      aes(x = age, y = working, group = factor(year)) +
      facet_wrap(~discipline, scales = "free_y") +
      geom_line(color = "gray") +
      geom_ribbon(
        data = object |> filter(discipline == d),
        aes(ymin = .lower, ymax = .upper, fill = factor(year)),
        alpha = 0.3
      ) +
      geom_line(
        data = object |> filter(discipline == d),
        aes(color = factor(year)), linewidth = 0.75
      ) +
      scale_x_continuous(breaks = seq(20, 100, by = 10)) +
      labs(
        x = "Age",
        y = "Number of active scientists",
        fill = "Forecast year"
      ) +
      guides(color = "none")
  })
  p[[length(p) + 1]] <- patchwork::guide_area()
  patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Forecast of working population by discipline",
    ) +
    patchwork::plot_layout(guides = "collect", axis_titles = "collect")
}
