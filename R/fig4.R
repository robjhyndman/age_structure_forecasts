make_fig4 <- function(retirements) {
  ggplot(retirements, aes(x = age_group, y = pc)) +
    geom_col(alpha = 0.6) +
    labs(
      title = "Retirement Intentions Across Key Industries",
      subtitle = "2022–23 Financial Year",
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
