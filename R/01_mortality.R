# Read mortality data from HMD

read_mortality <- function(mx_path, ex_path) {
  read_hmd_files(c(mx_path, ex_path)) |>
    filter(Sex == "Total", Year >= 1970) |>
    collapse_ages(max_age = 101) |>
    suppressWarnings() |>
    select(
      year = Year,
      age = Age,
      mortality = Mortality,
      exposures = Exposures
    ) |>
    as_vital(index = year, key = age, .age = "age", population = "exposures")
}

# Compute lifetable to convert mortality rates to probabilities
# Then smooth the result and add in population data
compute_death_prob <- function(mortality) {
  life_table(mortality) |>
    smooth_mortality(qx, k = 60) |>
    mutate(qx = c(.smooth)) |>
    select(year, age, qx) |>
    left_join(
      mortality |> select(year, age, population = exposures),
      by = c("year", "age")
    ) |>
    as_vital(index = year, key = age, .age = "age", populaton = "exposures")
}
