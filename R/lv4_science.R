# Course leavers

# Numbers of people at census
read_census <- function(file) {
  y2006 <- readxl::read_excel(file, sheet = "2006", skip = 10) |>
    janitor::clean_names() |>
    head(3535) |>
    transmute(
      discipline = non_school_qualification_field_of_study_qalfp_2_digit,
      qualification = non_school_qualification_level_of_education_qallp_1_digit,
      age_group = age_in_single_years_agep,
      persons = persons
    ) |>
    mutate(age_group = as.character(age_group))


  y2011 <- readxl::read_excel(file, sheet = "2011", skip = 10) |>
    janitor::clean_names() |>
    head(3010) |>
    transmute(
      discipline = qalfp_4_digit_level,
      qualification = qallp_1_digit_level,
      age_group = age_in_single_years_agep,
      persons = persons
    )


  y2016 <- readxl::read_excel(file,
    sheet = "2016 - Labour force flag",
    skip = 10
  ) |>
    janitor::clean_names() |>
    head(7070) |>
    transmute(
      discipline = qalfp_4_digit_level,
      participation = lffp_labour_force_participation_flag,
      qualification = qallp_1_digit_level,
      age_group = agep_age,
      persons = persons
    ) |>
    mutate(age_group = as.character(age_group))
  y2021 <- readxl::read_excel(file,
    sheet = "2021 - Labour force flag",
    skip = 10
  ) |>
    janitor::clean_names() |>
    head(7070) |>
    transmute(
      discipline = x4_digit_level_qalfp_non_school_qualification_field_of_study,
      participation = lffp_labour_force_participation_flag,
      qualification = x1_digit_level_qallp_non_school_qualification_level_of_education,
      age_group = agep_age,
      persons = persons
    ) |>
    mutate(age_group = as.character(age_group))

  # Combine 2006 and 2011 (which have no participation data)
  census <- bind_rows(
    y2006 |> mutate(Year = 2006L),
    y2011 |> mutate(Year = 2011L)
  ) |>
    filter(qualification != "Certificate Level") |>
    filter(qualification != "Advanced Diploma and Diploma Level") |>
    select(-qualification) |>
    mutate(
      age_group = stringr::str_remove(age_group, " years"),
      age_group = stringr::str_replace_all(age_group, " and over", "+"),
      age_group = ifelse(age_group %in% as.character(100:115), "100+", age_group)
    ) |>
    group_by(Year, age_group, discipline) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    select(Year, age_group, persons, discipline)

  # Combine 2016 and 2021 (split by labour force participation)
  censusp <- bind_rows(
    y2016 |> mutate(Year = 2016L),
    y2021 |> mutate(Year = 2021L)
  ) |>
    filter(qualification != "Certificate Level") |>
    filter(qualification != "Advanced Diploma and Diploma Level") |>
    select(-qualification) |>
    mutate(
      age_group = ifelse(age_group %in% as.character(100:115), "100+", age_group),
      participation = (participation == "Participates in the Labour Force")
    ) |>
    group_by(Year, age_group, participation, discipline) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    group_by(Year, age_group, discipline) |>
    summarise(
      participation = sum(participation * persons) / sum(persons),
      persons = sum(persons),
      .groups = "drop"
    ) |>
    mutate(participation = if_else(is.nan(participation), 0, participation))

  # Create an average participation rate from 2016 and 2021
  avg_participation <- censusp |>
    group_by(age_group, discipline) |>
    summarise(
      avg_participation = mean(participation),
      .groups = "drop"
    )

  # Add average participation rates for 2016 and 2021 to previous years
  census <- census |>
    left_join(avg_participation, by = c("age_group", "discipline")) |>
    mutate(participation = avg_participation) |>
    select(-avg_participation)

  # Combine all years
  census |>
    bind_rows(censusp) |>
    mutate(Working = persons * participation)
}

# Convert census data to single age groups and separate the disciplines
make_census_single_year <- function(census, course_leavers, completions, retirements, death_prob) {
  c2 <- census |> split(census$discipline)
  cl2 <- course_leavers |> split(course_leavers$discipline)

  output <- mapply(function(census_filtered, course_leavers_filtered) {
    census_filtered |>
      make_single_age(Working) |>
      as_vital(index = Year, key = Age, .age = "Age") |>
      cohort_interpolation() |>
      add_migrants(course_leavers_filtered, completions, retirements, death_prob) |>
      as_tibble() |>
      mutate(discipline = unique(census_filtered$discipline))
  }, c2, cl2, SIMPLIFY = FALSE)
  bind_rows(output) |>
    as_vital(index = Year, key = c(Age, discipline), .age = "Age")
}

# Forecast the working population for each discipline

forecast_pop <- function(
    census1, course_leavers,
    completions, retirements,
    death_prob, arma_coef_science,
    h, nsim) {
  c2 <- census1 |> split(census1$discipline)
  cl2 <- course_leavers |> split(course_leavers$discipline)
  output <- mapply(function(census_filtered, course_leavers_filtered) {
    forecast_pop_discipline(census_filtered, course_leavers_filtered,
      completions, retirements, death_prob, arma_coef_science,
      h = h, nsim = nsim
    ) |> as_tibble() |>
      mutate(discipline = unique(census_filtered$discipline))
  }, c2, cl2, SIMPLIFY = FALSE)

  output |>
    bind_rows() |>
    as_vital(index = Year, key = c(Age, discipline, .rep), .age = "Age")
}
