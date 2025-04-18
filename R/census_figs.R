make_fig2 <- function(census, subtitle) {
  years <- seq(2006, 2021, by = 5)
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF")
  names(cols) <- years
  census |>
    as_tibble() |>
    filter(year %in% years) |>
    mutate(Year = factor(year, levels = rev(years))) |>
    ggplot() +
    aes(x = age, y = working, color = Year, group = Year) +
    geom_line() +
    labs(y = "Number of active scientists") +
    scale_color_manual(values = cols, name = "Census Year") +
    labs(
      title = "Working Population",
      subtitle = subtitle
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10))
}

make_fig3 <- function(census, subtitle) {
  census |>
    autoplot(working) +
    labs(
      x = "Age",
      y = "Number of active scientists",
      title = "Interpolated Working Population",
      subtitle = paste0(subtitle, ": 2006 - 2021")
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10))
}
