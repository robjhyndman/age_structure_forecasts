make_future_Ext_fig <- function(object, data, h, times, yr) {
  object <- object |> 
    generate(h = h, times = times) |>
    filter(year == yr, age <= 100)
  if(NROW(object) == 0) {
    title <- "Remainder"
  } else {
    title <- paste("Simulated future remainder:", yr)
  }

  object |>
    ggplot() +
    geom_line(
      data = data |> filter(age <= 100),
      aes(x = age, y = remainder, group = year),
      color = "grey"
    ) +
    geom_line(aes(x = age, y = .sim, col = .rep, group = .rep)) +
    guides(color = "none") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Number of people"), # ($m_{x,t}$)"),
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}
