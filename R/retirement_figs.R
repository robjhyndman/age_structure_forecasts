make_fig4 <- function(retirements) {
  ggplot(retirements, aes(x = age_group, y = pc)) +
    geom_col(alpha = 0.6) +
    labs(
      title = "Retirement Intentions",
      subtitle = "2022--23 Financial Year",
      x = "Age group",
      y = "Percentage of retirement intentions"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

make_fig6 <- function(retirements, var) {
  retirements |>
    filter(Age < 100) |>
    ggplot() +
    aes(x = Age, y = {{ var }}) +
    geom_line() +
    labs(
      y = "Probability of retirement",
      title = "Probability of Retirement",
      subtitle = "Professional, scientific and technical services\n2022–23 Financial Year"
    ) +
    scale_x_continuous(breaks = seq(40, 100, by = 10)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}
