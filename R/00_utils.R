# -------------------------------------
# Utility functions used in the project
# -------------------------------------

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

quantile_df <- function(x, interval = c(95, 90, 80)) {
  tibble(
    lo = quantile(x, (1 - interval / 100) / 2, na.rm = TRUE),
    hi = quantile(x, 1 - (1 - interval / 100) / 2, na.rm = TRUE),
    interval = interval
  )
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
