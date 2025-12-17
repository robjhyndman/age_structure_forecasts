make_fig_r <- function(retirements) {
  ggplot(retirements, aes(x = age_group, y = pc)) +
    geom_col() +
    labs(
      title = "Retirement intentions of Australian scientists (2022-23)",
      x = "Age group",
      y = "Percentage of retirement intentions"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme(
      legend.position = "none"
    )
}

make_fig_rx <- function(retirements, retirement_data, add_line = TRUE) {
  retirement_data <- retirement_data |>
    mutate(
      lower = as.numeric(substr(age_group, 1, 2)),
      upper = as.numeric(substr(age_group, 4, 5)),
      upper = if_else(is.na(upper), 100, upper),
      nages = upper - lower + 1,
    )
  retirements <- retirements |>
    select(-pc) |>
    left_join(retirement_data, by = join_by(age == lower)) |>
    tidyr::fill(pc, nages, .direction = "down")
  p <- retirements |>
    ggplot() +
    aes(x = age) +
    geom_step(aes(y = pc / 100 / nages), color = "gray") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Probability of retirement"), # ($r_{x,t}$)"),
      title = "Probability of retirement for Australian scientists"
    ) +
    scale_x_continuous(breaks = seq(40, 100, by = 10)) +
    ylim(0, max(retirements$retire_prob))
  if (add_line) {
    p <- p + geom_line(aes(y = retire_prob), color = "black")
  }
  return(p)
}
