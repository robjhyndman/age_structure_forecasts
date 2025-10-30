# Course leavers

# Numbers of people at census
read_census <- function(file) {
  y2006 <- read_census_year(file, sheet = "2006")
  y2011 <- read_census_year(file, sheet = "2011")
  y2016 <- read_census_year(file, sheet = "2016 - Labour force flag")
  y2021 <- read_census_year(file, sheet = "2021 - Labour force flag")

  # Combine years
  census <- bind_rows(y2006, y2011, y2016, y2021) |>
    ave_smooth_pr() |>
    mutate(
      working = if_else(
        is.na(participation),
        persons * .smooth,
        persons * participation
      )
    )
  return(census)
}

# Convert census data to single age groups and separate the disciplines
make_census_single_year <- function(
  census,
  course_leavers,
  completions,
  retirements,
  death_prob
) {
  by_discipline <- "discipline" %in% names(census)
  if (by_discipline) {
    c2 <- census |> split(census$discipline)
    cl2 <- course_leavers |> split(course_leavers$discipline)
  } else {
    c2 <- list(census)
    cl2 <- list(
      course_leavers |>
        group_by(year) |>
        summarise(graduates = sum(graduates))
    )
  }
  output <- mapply(
    function(census_filtered, course_leavers_filtered) {
      out <- census_filtered |>
        make_single_age(working) |>
        as_vital(index = year, key = age, .age = "age") |>
        cohort_interpolation() |>
        add_migrants(
          course_leavers_filtered,
          completions,
          retirements,
          death_prob
        ) |>
        as_tibble()
      if (by_discipline) {
        out$discipline <- unique(census_filtered$discipline)
      }
      return(out)
    },
    c2,
    cl2,
    SIMPLIFY = FALSE
  )
  bind_rows(output) |>
    as_vital(
      index = year,
      key = if (by_discipline) c("age", "discipline") else "age",
      .age = "age"
    )
}

# Forecast the working population for each discipline

forecast_pop <- function(
  census1,
  course_leavers,
  completions,
  retirements,
  death_prob,
  arma_coef_science,
  h,
  nsim
) {
  c2 <- census1 |> split(census1$discipline)
  cl2 <- course_leavers |> split(course_leavers$discipline)
  output <- mapply(
    function(census_filtered, course_leavers_filtered) {
      forecast_pop_discipline(
        census_filtered,
        course_leavers_filtered,
        completions,
        retirements,
        death_prob,
        arma_coef_science,
        h = h,
        nsim = nsim
      ) |>
        as_tibble() |>
        mutate(discipline = unique(census_filtered$discipline))
    },
    c2,
    cl2,
    SIMPLIFY = FALSE
  )

  output |>
    bind_rows() |>
    as_vital(index = year, key = c(age, discipline, .rep), .age = "age")
}
