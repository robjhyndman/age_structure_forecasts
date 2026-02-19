plot_forecast_2021 <- function(forecast_error) {
  disciplines <- unique(forecast_error$discipline)
  p <- lapply(disciplines, function(d) {
    tmp <- forecast_error |>
      filter(discipline == d) |>
      rename(Actual = working, Forecast = forecast) |>
      tidyr::pivot_longer(
        c(Forecast, Actual),
        names_to = "type",
        values_to = "value"
      ) |>
      ggplot() +
      aes(x = age, y = value, linetype = type) +
      geom_line() +
      facet_wrap(~discipline, scales = "free_y") +
      guides(
        linetype = guide_legend(title = "")
      ) +
      labs(
        x = "Age",
        y = "Number of active scientists"
      )
  })
  p[[length(p) + 1]] <- patchwork::guide_area()
  out <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Forecast vs actual population of Australian scientists in 2021"
    ) +
    patchwork::plot_layout(axis_titles = "collect")
  out +
    patchwork::plot_layout(guides = "collect")
}
