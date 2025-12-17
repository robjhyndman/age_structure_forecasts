read_census_year <- function(file, sheet) {
  data <- readxl::read_excel(file, sheet = sheet, skip = 10) |>
    janitor::clean_names()
  # Find last row of data
  last_row <- which(stringr::str_detect(data[[1]], "Data source:"))
  data <- head(data, last_row - 1)
  empty_rows <- data[[1]] == "" | is.na(data[[1]])
  # Remove empty rows
  data <- data[!empty_rows, ]
  # Construct output tibble
  out <- tibble::tibble(
    year = rep(readr::parse_number(sheet), NROW(data))
  )
  # Age group
  out$age_group <- find_var(data, c("age_in_single_years_agep", "agep_age"))
  # Persons
  out$persons <- find_var(data, "persons")
  # Find category
  out$category <- find_var(
    data,
    c(
      "qalfp_2_digit_level",
      "non_school_qualification_field_of_study_qalfp_1_digit",
      "x2_digit_level_qalfp_non_school_qualification_field_of_study"
    )
  )
  # Find discipline
  out$discipline <- find_var(
    data,
    c(
      "qalfp_4_digit_level",
      "non_school_qualification_field_of_study_qalfp_2_digit",
      "x4_digit_level_qalfp_non_school_qualification_field_of_study"
    )
  )
  # Find qualification
  out$qualification <- find_var(
    data,
    c(
      "qallp_1_digit_level",
      "non_school_qualification_level_of_education_qallp_1_digit",
      "x1_digit_level_qallp_non_school_qualification_level_of_education"
    )
  )
  # Find participation flag
  out$participation <- find_var(data, "lffp_labour_force_participation_flag")
  if (!is.null(out[["participation"]])) {
    out$participation <- out$participation == "Participates in the Labour Force"
  }

  # Only scientists
  if (!is.null(out[["category"]])) {
    out <- out |> filter(category == "Natural and Physical Sciences")
    out$category <- NULL
  }
  # Only bachelor degrees and above
  if (!is.null(out[["qualification"]])) {
    out <- out |>
      filter(
        !qualification %in%
          c("Certificate Level", "Advanced Diploma and Diploma Level")
      )
    #out$qualification <- NULL
  }

  # Combine "Other Natural and Physical Sciences" and "Natural and Physical Sciences (n.f.d.)"
  if (!is.null(out[["discipline"]])) {
    out$discipline <- if_else(
      out$discipline == "Natural and Physical Sciences (n.f.d.)" |
        out$discipline == "Natural and Physical Sciences, nfd",
      "Other Natural and Physical Sciences",
      out$discipline
    )
  }
  # Ages
  out$age_group <- as.character(out$age_group)
  out$age_group <- stringr::str_remove(out$age_group, " years")
  out$age_group <- stringr::str_replace_all(
    out$age_group,
    " and over",
    "+"
  )
  out$age_group = ifelse(
    out$age_group %in% as.character(100:115),
    "100+",
    out$age_group
  )
  # Sum within each age group and discipline
  gps <- c("age_group", "year", "participation", "discipline")
  gps <- gps[gps %in% colnames(out)]
  out <- out |>
    group_by(across(all_of(gps))) |>
    summarise(persons = sum(persons), .groups = "drop")

  # Calculate participation rates by discipline and age group
  if (!is.null(out[["participation"]])) {
    gps <- gps[gps != "participation"]
    out <- out |>
      group_by(across(all_of(gps))) |>
      summarise(
        participation = sum(participation * persons) / sum(persons),
        persons = sum(persons),
        .groups = "drop"
      ) |>
      mutate(participation = if_else(is.nan(participation), 0, participation))
  }

  # Reorder columns
  out |>
    mutate(age = readr::parse_number(age_group)) |>
    arrange(age) |>
    select(year, age_group, age, persons, everything())
}

find_var <- function(data, possible_names) {
  col <- colnames(data)
  col <- col[col %in% possible_names]
  if (length(col) > 0) {
    if (length(col) > 1) {
      warning(
        "Multiple columns found for ",
        paste(possible_names, collapse = ", ")
      )
    }
    data[[col[1]]]
  } else {
    NULL
  }
}


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
  df,
  graduates,
  ave_completions,
  sd_completions,
  retirements,
  death_prob,
  arma_coef_science,
  h = 20,
  nsim = 500
) {
# Last year of available population data
  last_yr <- max(df$year)

  # Estimate future migrants in last year
  # (since we can't compute them using following year)
  fit_migrants <- df |>
    filter(year < last_yr, !is.na(graduates)) |>
    make_sd(remainder, key = discipline) |>
    model(fdm = FDM(remainder, coherent = TRUE))
  last_yr_migrants <- fit_migrants |>
    generate(h = 1, times = nsim) |>
    as_tibble() |>
    select(-.model, remainder = .sim)
  # Add mean to other disciplines
  lym <- left_join(
    last_yr_migrants |> as_tibble() |> filter(discipline != "mean"),
    last_yr_migrants |> as_tibble() |> filter(discipline == "mean"),
    by = c("year", "age", ".rep")
  ) |>
  mutate(
    remainder = remainder.x + remainder.y,
    discipline = discipline.x
  ) |>
  select(year, age, discipline, .rep, remainder) 

  # Add migrants into last year
  if("discipline" %in% colnames(df)) {
    joinby <- c("age", "year", "discipline")
  } else {
    joinby <- c("age", "year")
  }
  population <- df |>
    filter(year == last_yr) |>
    select(-remainder) |>
    as_tibble() |>
    left_join(last_yr_migrants, by = joinby)

  # Forecast mortality rates
  future_death_prob <- mortality |>
    model(fdm = FDM(log(qx))) |>
    generate(h = h, times = nsim) |>
    rename(mortality = .sim) |>
    select(-.model)

  # Simulate future migrant numbers
  future_migrants <- fit_migrants |>
    generate(h = h + 1, times = nsim) |>
    filter(year > last_yr) |>
    rename(remainder = .sim) |>
    select(-.model)

  # Simulate future graduate numbers
  future_graduates <- graduates |>
    fit_global_model(arma_coef) |>
    generate(h = h, times = nsim) |>
    rename(graduates = .sim) |>
    select(-.model)
  # Add actual graduate numbers where available
  future_graduates <- future_graduates |>
    bind_rows(
      graduates |>
        filter(year >= last_yr) |>
        expand_grid(.rep = unique(future_graduates$.rep))
    )
  # Simulated completions
  sim_completions <- expand_grid(
    .rep = as.character(seq(nsim)),
    age = ave_completions$age,
  ) |>
    left_join(ave_completions, by = "age") |>
    rename(mean = pc) |>
    left_join(sd_completions, by = "age") |>
      rename(sd = pc) 
  sim_completions <- sim_completions |>
    mutate(
      pc = rnorm(NROW(sim_completions), mean = mean, sd = sd),
      pc = pmax(0, pmin(100, pc))
    ) |>
    select(.rep, age, pc) |>
    group_by(.rep) |>
    mutate(
      pc = pc / sum(pc) * sum(ave_completions$pc)
    ) |>
    ungroup()
  
  # Disaggregate by age
  N <- expand_grid(
    year = last_yr + seq(h),
    age = unique(sim_completions$age),
    .rep = unique(future_graduates$.rep)
  ) |>
    left_join(future_graduates, by = c(".rep", "year")) |>
    left_join(sim_completions, by = c(".rep","age")) |>
    mutate(
      pc = if_else(is.na(pc), 0, pc),
      graduates = pc * graduates / 100
    ) |>
    select(-pc)

  N <- N |>
    left_join(future_death_prob, by = c("age", "year", ".rep")) |>
    left_join(retirements |> select(-pc), by = "age") |>
    mutate(retire_prob = if_else(age < 45, 0, retire_prob)) |>
    left_join(future_migrants, by = c("age", "year", ".rep")) |>
    filter(year <= last_yr + h)

  # Get first year working population of simulation
  tmp <- population |>
    transmute(
      .rep = .rep,
      age = age + 1,
      year = year + 1,
      working = pmax(0, working - deaths - retirees + graduates + remainder),
      working = round(working)
    ) |>
    filter(age <= 100) |>
    bind_rows(
      tibble(age = 15, year = last_yr + 1, working = 0),
    )

  N <- N |>
    left_join(tmp, by = c("age", "year", ".rep")) |>
    mutate(working = if_else(is.na(working), 0, working))

  # Now iterate to generate population each year
  # Pull out year to process
  for (i in 2:h) {
    N1 <- N |>
      filter(year == last_yr + i) |>
      select(-working)
    N2 <- N |> filter(year != last_yr + i)
    # Compute migrants
    next_year <- N |>
      filter(year == last_yr + i - 1) |>
      transmute(
        .rep = .rep,
        working = if_else(age <= 15, 0, working),
        year = year + 1,
        age = age + 1
      )
    N1 <- N1 |>
      left_join(next_year, by = c("age", "year", ".rep")) |>
      mutate(working = if_else(is.na(working), 0, working)) |>
      mutate(
        deaths = rbinom(
          n = NROW(N1),
          size = working,
          prob = mortality
        ),
        retirees = rbinom(
          n = NROW(N1),
          size = working - deaths,
          prob = retire_prob
        ),
        working = pmax(0, working - deaths - retirees + graduates + remainder),
        working = if_else(age == 15, 0, round(working)),
      ) |>
      filter(age <= 100) |>
      select(-deaths, -retirees)
    N <- bind_rows(N1, N2)
  }
  N |>
    select(year, age, .rep, working) |>
    as_vital(index = year, key = c(age, .rep), .age = "age")


  output |>
    as_vital(index = year, key = c(age, discipline, .rep), .age = "age")
}
