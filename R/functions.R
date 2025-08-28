# -----------------------------
# Functions used in the project
# -----------------------------

# Function to make input vector monotonically increasing
# Used to correct minor deviations from monotonicity
make_monotonic <- function(x) {
  for (i in 2:length(x)) {
    if (x[i] < x[i - 1]) {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

# Disaggregate data into single year age groups without smoothing
aas_step <- function(x, y, xout) {
  fit <- approx(x = x, y = y, method = "constant", xout = xout, rule = 2) |>
    suppressWarnings()
  fit$y
}

# Disaggregate data into single year age groups with smoothing
# Use monotonic smoothing of cumulative values so differences stay positive.
aas_smooth <- function(x, y, xout) {
  fit <- spline(x = x, y = cumsum(y), method = "hyman", xout = xout) |>
    suppressWarnings()
  diff(fit$y)
}

# As we only have population data at the census dates, we will interpolate
# the population for the years between censuses using a cohort linear interpolation method
cohort_interpolation <- function(df) {
  fullP <- expand_grid(year = 2006:2021, age = 15:100) |>
    mutate(birth_year = year - age) |>
    left_join(df, by = c("year", "age")) |>
    select(-age)
  fullP <- lapply(
    split(fullP, fullP$birth_year),
    function(x) {
      if (sum(!is.na(x$working)) <= 1 | NROW(x) <= 1) {
        return(x)
      } else {
        x$working <- approx(
          x = x$year,
          y = x$working,
          method = "linear",
          xout = x$year,
          rule = 2
        )$y
        return(x)
      }
    }
  ) |>
    bind_rows() |>
    mutate(
      working = if_else(is.na(working), 0, working),
      age = year - birth_year
    ) |>
    select(-birth_year) |>
    as_vital(index = year, key = age, .age = "age")

  return(fullP)
}

# Take a tibble with age groups and make it a single year of age tibble
# Assumes there is a column "age_group" of the form lower-upper, or "lower+"
# specifying each age group.
# Ages below lowest age and above highest age are omitted
# Numbers in each age group are distributed proportionally across all ages.
# If smooth, the resulting step function is smoothed using positive cubic splines
# Will group by year if there is a variable called "year"

make_single_age <- function(
  df,
  variable,
  min_age = 15,
  max_age = 100,
  smooth = TRUE
) {
  var_name <- deparse(substitute(variable))
  groups <- "year" %in% colnames(df)
  # Add lower and upper age columns
  df$lower_age <- readr::parse_number(stringr::str_extract(
    df$age_group,
    "^[0-9]*"
  ))
  df$upper_age <- readr::parse_number(stringr::str_extract(
    df$age_group,
    "\\d+(?!.*\\d)"
  ))
  df$upper_age <- if_else(
    df$upper_age == max(df$upper_age),
    max_age,
    df$upper_age
  )
  if (groups) {
    output <- list()
    df <- split(df, df$year)
    for (i in seq_along(df)) {
      output[[i]] <- make_single_age_oneyear(
        df[[i]],
        var_name,
        min_age,
        max_age,
        smooth
      )
    }
    output <- bind_rows(output)
  } else {
    output <- make_single_age_oneyear(df, var_name, min_age, max_age, smooth)
  }
  # Return tibble
  output
}

# This function does the work for a single year of data
make_single_age_oneyear <- function(
  df,
  var_name,
  min_age = 15,
  max_age = 100,
  smooth = TRUE
) {
  # df should only contain the data for one year.
  # Therefore age_groups should be unique
  if (length(unique(df$lower_age)) != nrow(df)) {
    stop("Age groups are not unique")
  }
  # Sort results
  df <- df |> arrange(upper_age)
  # Set up output tibble
  ages <- seq(min_age, max_age, by = 1L)
  output <- tibble(age = ages, y = 0)
  colnames(output) <- c("age", var_name)
  if ("year" %in% colnames(df)) {
    output$year <- df$year[1]
    output <- output[, c("year", "age", var_name)]
  }
  # Either use a monotonic smoothing spline to cumulative data, or use a step function
  if (smooth) {
    output[[var_name]] <- aas_smooth(
      x = c(min(df$lower_age) - 1, df$upper_age),
      y = c(0, df[[var_name]]),
      xout = c(min_age - 1, ages)
    )
  } else {
    df[[var_name]] <- df[[var_name]] / (df$upper_age - df$lower_age + 1)
    output[[var_name]] <- aas_step(
      x = c(df$lower_age, df$upper_age),
      y = c(df[[var_name]], df[[var_name]]),
      xout = ages
    )
  }
  return(output)
}

# Function to add graduates, retirees, deaths and migrants
add_migrants <- function(df, graduates, completions, retirements, mortality) {
  # Construct new graduate age distribution
  N <- expand_grid(year = graduates$year, age = completions$age) |>
    left_join(graduates, by = "year") |>
    left_join(completions, by = "age") |>
    mutate(
      pc = if_else(is.na(pc), 0, pc),
      graduates = pc * graduates / 100
    ) |>
    select(-pc) |>
    as_vital(index = year, key = age, .age = "age")

  # Add graduates, deaths and retirements to population data
  P <- df |>
    left_join(N, by = c("year", "age")) |>
    left_join(mortality, by = c("age", "year")) |>
    left_join(retirements, by = "age") |>
    mutate(
      retire_prob = if_else(is.na(retire_prob), 0, retire_prob),
      retirees = retire_prob * working,
      deaths = qx * working
    ) |>
    select(year, age, working, graduates, retirees, deaths)
  # Compute migrants
  next_year <- P |>
    transmute(
      lead_working = if_else(age <= 15, 0, working),
      year = year - 1,
      age = age - 1
    )
  P <- P |>
    left_join(next_year, by = c("year", "age")) |>
    mutate(
      migrants = lead_working - working + deaths + retirees - graduates,
      migrants = if_else(age == max(age), 0, migrants)
    ) |>
    select(-lead_working) |>
    as_vital(index = year, key = age, .age = "age")

  # Rename migrants as remainder
  P <- P |>
    rename(remainder = migrants)

  # Return complete data
  return(P)
}

# Forecast function

forecast_pop_discipline <- function(
  df,
  graduates,
  completions,
  retirements,
  mortality,
  arma_coef,
  h = 20,
  nsim = 500
) {
  # Last year of available population data
  last_yr <- max(df$year)

  # Estimate future migrants in last year
  # (since we can't compute them using following year)
  fit_migrants <- df |>
    filter(year < last_yr, !is.na(graduates)) |>
    model(fdm = FDM(remainder, coherent = TRUE))
  last_yr_migrants <- fit_migrants |>
    forecast(h = 1) |>
    as_tibble() |>
    select(-remainder, -.model) |>
    rename(remainder = .mean)
  # Add migrants into last year
  population <- bind_rows(
    df |> filter(year < last_yr),
    df |>
      filter(year == last_yr) |>
      select(-remainder) |>
      left_join(
        last_yr_migrants,
        by = c("age", "year")
      )
  )

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

  # Disaggregate by age
  N <- expand_grid(
    year = last_yr + seq(h),
    age = completions$age,
    .rep = unique(future_graduates$.rep)
  ) |>
    left_join(future_graduates, by = c(".rep", "year")) |>
    left_join(completions, by = "age") |>
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
    filter(year == last_yr) |>
    transmute(
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
    left_join(tmp, by = c("age", "year")) |>
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
}

# Function to simulate future graduates only
simulate_future_graduates <- function(
  graduates,
  arma_coef,
  h = 18,
  nsim = 500
) {
  future_graduates <- graduates |>
    fit_global_model(arma_coef) |>
    generate(h = h, times = nsim) |>
    rename(graduates = .sim) |>
    select(-.model)

  last_yr <- max(graduates$year)
  future_graduates <- future_graduates |>
    bind_rows(
      graduates |>
        filter(year >= last_yr) |>
        expand_grid(.rep = unique(future_graduates$.rep))
    )

  return(future_graduates)
}

# Fit Global ARIMA model to course leavers data
global_arma <- function(course_leavers) {
  # Find mean per discipline
  mean_grads <- course_leavers |>
    group_by(discipline) |>
    summarise(
      mean_grads = mean(graduates),
      mean_diff = mean(tsibble::difference(graduates), na.rm = TRUE)
    )

  # Scale course leavers by means and take differences, removing any drift
  scaled_course_leavers <- course_leavers |>
    left_join(mean_grads, by = "discipline") |>
    group_by(discipline) |>
    mutate(
      graduates = (tsibble::difference(graduates) - mean_diff) / mean_grads
    )

  # Construct single series of all graduates with large missing sections to wash out memory
  y <- scaled_course_leavers |>
    bind_rows(
      expand_grid(
        year = 1960:(min(course_leavers$year) - 1),
        discipline = unique(course_leavers$discipline)
      )
    ) |>
    arrange(discipline, year) |>
    pull(graduates)

  # Fit ARIMA model with d = 0 to differenced data
  global_fit <- tibble(y = y) |>
    mutate(t = row_number()) |>
    as_tsibble(index = t) |>
    model(arima = ARIMA(y ~ 0 + pdq(p = 1, d = 0)))

  # Grab ARMA coefficients
  tidy(global_fit) |>
    filter(stringr::str_detect(term, "ar") | stringr::str_detect(term, "ma")) |>
    select(term, estimate)
}

fit_global_model <- function(graduates, coef) {
  ar <- stringr::str_detect(coef$term, "ar")
  ma <- stringr::str_detect(coef$term, "ma")
  p <- sum(ar)
  q <- sum(ma)
  if ("discipline" %in% colnames(graduates)) {
    tsbl <- graduates |>
      as_tsibble(index = year, key = discipline)
  } else {
    tsbl <- graduates |>
      as_tsibble(index = year)
  }
  tsbl |>
    model(
      arima = ARIMA(
        graduates ~ 1 + pdq(p = p, d = 1, q = q),
        fixed = c(coef$estimate, NA),
        transform.pars = FALSE
      )
    )
}

ave_smooth_pr <- function(data) {
  if ("discipline" %in% colnames(data)) {
    data <- split(data, data$discipline)
  } else {
    data <- list(data)
  }
  smooth <- lapply(data, function(x) {
    fit <- cobs::cobs(
      x$age,
      log(x$participation + 0.001),
      constraint = "concave",
      lambda = 0.5,
      print.warn = FALSE,
      print.mesg = FALSE
    )
    age <- sort(unique(x$age))
    data.frame(
      age = age,
      .smooth = exp(predict(
        fit,
        minz = min(age),
        maxz = max(age),
        nz = length(age)
      )[, 2])
    )
  })
  purrr::map2_dfr(data, smooth, function(x, y) {
    x |> left_join(y, by = "age")
  })
}

make_discipline_table <- function() {
  tibble::tibble(
    Narrow_Discipline = c(
      "Physics and Astronomy",
      "Mathematical Sciences",
      "Chemical Sciences",
      "Earth Sciences",
      "Biological Sciences",
      "Other Natural and Physical Sciences"
    ),
    Detailed_Field = c(
      "Physics, Astronomy.",
      "Mathematics, Statistics, Mathematical Sciences, n.e.c.",
      "Organic Chemistry, Inorganic Chemistry, Chemical Sciences, n.e.c.",
      "Atmospheric Sciences, Geology, Geophysics, Geochemistry, Soil Science, Hydrology, Oceanography, Earth Sciences, n.e.c.",
      "Biochemistry and Cell Biology, Botany, Ecology and Evolution, Marine Science, Genetics, Microbiology, Human Biology, Zoology, Biological Sciences, n.e.c.",
      "Medical Science, Forensic Science, Food Science and Biotechnology, Pharmacology, Laboratory Technology, Natural and Physical Sciences, n.e.c."
    )
  ) |>
    kableExtra::kbl(
      format = "latex",
      col.names = c("Narrow Fields", "Detailed Fields"),
      booktabs = TRUE,
      linesep = ""
    ) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::column_spec(2, width = "9cm")
}
