make_fig3 <- function(census) {
  census |>
  autoplot(Working) +
  labs(y = "Number of active scientists") +
  labs(
    title = "Interpolated Working Population",
    subtitle = "Natural and Physical Sciences\n2006 - 2021"
  ) +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  theme_classic()  +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
}
