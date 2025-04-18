make_fig1 <- function(census_science) {
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF", "#000000")
  names(cols) <- c(unique(census_science$year), "Average")

  census_science <- census_science |>
    mutate(
      age_group = forcats::fct_relevel(age_group, "100+", after = Inf),
      year = factor(year, levels = c(2006, 2011, 2016, 2021))
    )

  census_science |>
    filter(year %in% c("2016", "2021")) |>
    ggplot(
      aes(
        x = as.numeric(gsub("\\+", "", age_group)),
        y = participation,
        shape = as.factor(year),
        color = as.factor(year)
      )
    ) +
    geom_point(size = 1.5) +
    geom_line(aes(y = .smooth, color = "black")) +
    labs(
      title = "Labour Force Participation",
      subtitle = "Natural and Physical Sciences",
      x = "One-year age groups",
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
