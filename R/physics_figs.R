make_fig13 <- function(physics, subtitle) {
  physics |>
    ggplot(aes(x = Year, y = Graduates)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
    labs(
      title = "Graduates",
      subtitle = paste0(subtitle, "\n2006–2023"),
      y = "Number of graduates"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

make_fig14 <- function(future_physics_grads, physics_grads) {
  future_physics_grads <- future_physics_grads |>
    summarise(
      mean = mean(Graduates),
      lo = quantile(Graduates, prob = 0.05),
      hi = quantile(Graduates, prob = 0.95)
    )
  ggplot(future_physics_grads) +
    aes(x = Year) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = physics_grads, aes(y = Graduates)) +
    labs(
      y = "Number of graduates",
      title = "Forecast of Graduates",
      subtitle = "Physics and Astronomy\nForecasted Years: 2024–2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

make_fig15 <- function(physics) {
physics |>
  autoplot(Migrants) +
  scale_x_continuous(breaks = seq(10,100,by=10)) +
  labs(
    y = "Number of net migrants",
    title = "Estimated Net Migration",
    subtitle = "Physics and Astronomy\n2006 - 2020"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) +
  theme_classic()  +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
}
