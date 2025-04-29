# Read mortality data from HMD

read_mortality <- function(mx_path, ex_path) {
  read_hmd_files(c(mx_path, ex_path)) |>
    filter(Sex == "Total", Year >= 1970) |>
    collapse_ages() |>
    suppressWarnings() |>
    select(
      year = Year,
      age = Age,
      mortality = Mortality,
      exposures = Exposures
    ) |>
    as_vital(index = year, key = age, .age = "age", populaton = "exposures")
}

# Compute lifetable to convert mortality rates to probabilities
# Then smooth the result and add in population data
compute_death_prob <- function(mortality) {
  life_table(mortality) |>
    smooth_mortality(qx, k = 50) |>
    mutate(death_prob = c(.smooth)) |>
    select(year, age, death_prob) |>
    left_join(
      mortality |> select(year, age, population = exposures),
      by = c("year", "age")
    ) |>
    as_vital(index = year, key = age, .age = "age", populaton = "exposures")
}


# Graph showing the probability of death by age over time
make_fig_mxt <- function(death_prob) {
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
