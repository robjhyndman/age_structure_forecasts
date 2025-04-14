make_fig8 <- function(death_prob) {
  death_prob |>
    autoplot(death_prob) +
    scale_y_log10(labels = scales::label_number()) +
    labs(y = "Log-scaled probability of death") +
    labs(
      title = "Australian Mortality-Based Death Probabilities",
      subtitle = "1971 - 2021"
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}
