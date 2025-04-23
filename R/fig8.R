make_fig8 <- function(death_prob) {
  death_prob |>
    autoplot(death_prob) +
    scale_y_log10(labels = scales::label_number()) +
    labs(
      y = "Probability of death",
      x = "Age",
      title = "Australian Death Probabilities",
      subtitle = "1971 – 2021"
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}
