make_fig8 <- function(death_prob) {
  death_prob |>
    autoplot(death_prob) +
    scale_y_log10(labels = scales::label_number()) +
    labs(
      y = latex2exp::TeX("Probability of death ($m_{x,t}$)"),
      x = "Age",
      title = "Probability of death for Australians (1971 – 2021)",
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}
