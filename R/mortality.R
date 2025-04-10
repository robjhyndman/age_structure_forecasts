# Read mortality data from HMD

read_mortality <- function(mx_path, ex_path) {
  read_hmd_files(c(mx_path, ex_path)) |>
    filter(Sex == "Total", Year > 1970) |>
    collapse_ages() |>
    suppressWarnings() |>
    select(-Sex, -OpenInterval)
}

# Compute lifetable to convert mortality rates to probabilities
# Then smooth the result and add in population data
compute_death_prob <- function(mortality) {
  life_table(mortality) |>
    smooth_mortality(qx, k = 20) |>
    mutate(death_prob = c(.smooth)) |>
    select(Year, Age, death_prob) |>
    left_join(
      mortality |> select(Year, Age, population = Exposures),
      by = c("Year", "Age")
    ) |>
    as_vital(index = Year, key = Age, .age = "Age")
}
