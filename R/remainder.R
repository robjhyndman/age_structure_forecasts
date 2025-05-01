make_future_Ext_fig <- function(object, data, h, times, yr) {
  object |>
    generate(h = h, times = times) |>
    filter(year == yr, age <= 100) |>
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
      y = latex2exp::TeX("Remainder"), # ($m_{x,t}$)"),
      title = paste("Simulated future remainder for", yr)
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) 
}