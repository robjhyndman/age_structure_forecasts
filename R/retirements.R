# Age distribution of retirement intentions

read_retirements <- function(file) {
  readxl::read_excel(file, sheet = "Retirement intentions by occ", skip = 10) |>
    janitor::clean_names() |>
    head(56) |>
    filter(
      x1 %in%
        c(
          "Professional, scientific and technical services",
          "Education and training",
          "Health care and social assistance"
        )
    ) |>
    select(contains("years")) |>
    mutate(across(
      everything(),
      function(x) as.numeric(x) * c(15.81, 15.48, 14.65) / 100
    )) |>
    summarise(across(everything(), sum)) |>
    tidyr::pivot_longer(
      everything(),
      names_to = "age_group",
      values_to = "count"
    ) |>
    transmute(
      age_group = stringr::str_remove(age_group, "^x"),
      age_group = stringr::str_remove(age_group, "_years"),
      age_group = stringr::str_replace(age_group, "_and_over", "+"),
      age_group = stringr::str_replace_all(age_group, "_", "-"),
      count = as.numeric(count),
      pc = count / sum(count) * 100,
    ) |>
    select(age_group, pc)
}

single_age_retirements <- function(retirement_data, death_prob) {
  retirement <- retirement_data |>
    make_single_age(pc, min_age = 45)

  # retirement |> ggplot(aes(x=age, y=cumsum(pc))) + geom_line()
  # retirement |> ggplot(aes(x=age, y=pc)) + geom_line()

  # That looks weird below 55. Let's just replace that section by a linear interpolation

  retirement <- retirement |>
    mutate(
      pc = if_else(age <= 59, (age - 44) * 0.0727505, pc),
      pc = pc / sum(pc) * 100
    )

  # retirement |> ggplot(aes(x=age, y=pc)) + geom_line()
  # Looks better

  # Estimate retirement rates by age from the retirement age distribution
  # Use the general Australian population in last year of available data

  retirement <- retirement |>
    left_join(death_prob |> filter(year == max(year)), by = "age") |>
    mutate(
      population = 100 * population / mean(population),
      dx = (pc / 100) * population,
      mx = dx / population,
      qx = mx / (1 + 0.5 * mx),
      #qx = make_monotonic(qx),
      #qx = if_else(death_prob + qx > 1, 1 - death_prob, qx)
    ) |>
    select(age, pc, retire_prob = qx)

  return(retirement)
}

make_fig_r <- function(retirements) {
  ggplot(retirements, aes(x = age_group, y = pc)) +
    geom_col() +
    labs(
      title = "Retirement intentions of Australian scientists (2022 - 23)",
      x = "Age group",
      y = "Percentage of retirement intentions"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme(
      legend.position = "none"
    )
}

make_fig_rx <- function(retirements, retirement_data) {
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
  retirements |>
    ggplot() +
    aes(x = age) +
    geom_step(aes(y = pc / 100 / nages), color = "gray") +
    geom_line(aes(y = retire_prob), color = "black") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Probability of retirement"), # ($r_{x,t}$)"),
      title = "Probability of retirement for Australian scientists"
    ) +
    scale_x_continuous(breaks = seq(40, 100, by = 10))
}
