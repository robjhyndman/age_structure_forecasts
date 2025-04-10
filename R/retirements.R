# Script to read in age distribution of retirement intentions
# mortality.R needs to be run first to get Australian population data

read_retirements <- function(file) {
  readxl::read_excel(file, sheet = "Retirement intentions by occ", skip = 10) |>
    janitor::clean_names() |>
    head(56) |>
    filter(x1 == "Professional, scientific and technical services") |>
    select(contains("years")) |>
    tidyr::pivot_longer(everything(), names_to = "age_group", values_to = "count") |>
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

  # retirement |> ggplot(aes(x=Age, y=cumsum(pc))) + geom_line()
  # retirement |> ggplot(aes(x=Age, y=pc)) + geom_line()

  # That looks weird below 55. Let's just replace that section by a linear interpolation

  retirement <- retirement |>
    mutate(pc = if_else(Age <= 59, (Age - 44) * 0.0727505, pc))

  # retirement |> ggplot(aes(x=Age, y=pc)) + geom_line()
  # Looks better

  # Estimate retirement rates by age from the retirement age distribution
  # Use the general Australian population in last year of available data

  retirement <- retirement |>
    left_join(death_prob |> filter(Year == max(Year)), by = "Age") |>
    mutate(
      population = 100 * population / mean(population),
      dx = (pc / 100) * population,
      mx = dx / population,
      qx = if_else(Age == max(Age), 1, mx / (1 + 0.5 * mx)),
      qx = make_monotonic(qx),
      qx = if_else(death_prob + qx > 1, 1 - death_prob, qx)
    ) |>
    select(Age, pc, retire_prob = qx)

  return(retirement)
}
