make_fig2 <- function(census) {
  years <- seq(2006, 2021, by = 5)
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF")
  names(cols) <- years
  census |>
    as_tibble() |>
    filter(Year %in% years) |>
    mutate(Year = factor(Year, levels = rev(years))) |>
    ggplot() +
    aes(x = Age, y = Working, color = Year, group = Year) +
    geom_line() +
    labs(y = "Number of active scientists") +
    scale_color_manual(values = cols, name = "Census Year") +
    labs(
      title = "Working Population",
      subtitle = "Natural and Physical Sciences"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}
