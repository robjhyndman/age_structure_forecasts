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
  fullP <- expand_grid(Year = 2006:2021, Age = 15:100) |>
    mutate(birth_year = Year - Age) |>
    left_join(df, by = c("Year", "Age")) |>
    select(-Age)
  fullP <- lapply(
    split(fullP, fullP$birth_year),
    function(x) {
      if (sum(!is.na(x$Working)) <= 1 | NROW(x) <= 1) {
        return(x)
      } else {
        x$Working <- approx(x = x$Year, y = x$Working, method = "linear", xout = x$Year, rule = 2)$y
        return(x)
      }
    }
  ) |>
    bind_rows() |>
    mutate(
      Working = if_else(is.na(Working), 0, Working),
      Age = Year - birth_year
    ) |>
    select(-birth_year) |>
    as_vital(index = Year, key = Age, .age = "Age")

  return(fullP)
}

# Take a tibble with age groups and make it a single year of age tibble
# Assumes there is a column "age_group" of the form lower-upper, or "lower+"
# specifying each age group.
# Ages below lowest age and above highest age are omitted
# Numbers in each age group are distributed proportionally across all ages.
# If smooth, the resulting step function is smoothed using positive cubic splines
# Will group by year if there is a variable called "year"

make_single_age <- function(df, variable, min_age = 15, max_age = 100, smooth = TRUE) {
  var_name <- deparse(substitute(variable))
  groups <- "Year" %in% colnames(df)
  # Add lower and upper age columns
  df$lower_age <- readr::parse_number(stringr::str_extract(df$age_group, "^[0-9]*"))
  df$upper_age <- readr::parse_number(stringr::str_extract(df$age_group, "\\d+(?!.*\\d)"))
  df$upper_age <- if_else(df$upper_age == max(df$upper_age), max_age, df$upper_age)
  if (groups) {
    output <- list()
    df <- split(df, df$Year)
    for (i in seq_along(df)) {
      output[[i]] <- make_single_age_oneyear(df[[i]], var_name, min_age, max_age, smooth)
    }
    output <- bind_rows(output)
  } else {
    output <- make_single_age_oneyear(df, var_name, min_age, max_age, smooth)
  }
  # Return tibble
  output
}

# This function does the work for a single year of data
make_single_age_oneyear <- function(df, var_name, min_age = 15, max_age = 100, smooth = TRUE) {
  # df should only contain the data for one year.
  # Therefore age_groups should be unique
  if (length(unique(df$lower_age)) != nrow(df)) {
    stop("Age groups are not unique")
  }
  # Sort results
  df <- df |> arrange(upper_age)
  # Set up output tibble
  ages <- seq(min_age, max_age, by = 1L)
  output <- tibble(Age = ages, y = 0)
  colnames(output) <- c("Age", var_name)
  if ("Year" %in% colnames(df)) {
    output$Year <- df$Year[1]
    output <- output[, c("Year", "Age", var_name)]
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
  N <- expand_grid(Year = graduates$Year, Age = completions$Age) |>
    left_join(graduates, by = "Year") |>
    left_join(completions, by = "Age") |>
    mutate(
      pc = if_else(is.na(pc), 0, pc),
      Graduates = pc * Graduates / 100
    ) |>
    select(-pc) |>
    as_vital(index = Year, key = Age, .age = "Age")

  # Add graduates, deaths and retirements to population data
  P <- df |>
    left_join(N, by = c("Year", "Age")) |>
    left_join(mortality, by = c("Age", "Year")) |>
    left_join(retirements, by = "Age") |>
    mutate(
      retire_prob = if_else(is.na(retire_prob), 0, retire_prob),
      Retirees = retire_prob * Working,
      Deaths = death_prob * Working
    ) |>
    select(Year, Age, Working, Graduates, Retirees, Deaths)
  # Compute migrants
  next_year <- P |>
    transmute(
      Lead_Working = if_else(Age <= 15, 0, Working),
      Year = Year - 1,
      Age = Age - 1
    )
  P <- P |>
    left_join(next_year, by = c("Year", "Age")) |>
    mutate(
      Migrants = Lead_Working - Working + Deaths + Retirees - Graduates,
      Migrants = if_else(Age == max(Age), 0, Migrants)
    ) |>
    select(-Lead_Working) |>
    as_vital(index = Year, key = Age, .age = "Age")

  # Return complete data
  return(P)
}

# Forecast function

forecast_pop_discipline <- function(df, graduates, completions, retirements, mortality, arma_coef, h = 20, nsim = 500) {
  # Last year of available population data
  last_yr <- max(df$Year)

  # Estimate future migrants in last year
  # (since we can't compute them using following year)
  fit_migrants <- df |>
    filter(Year < last_yr, !is.na(Graduates)) |>
    model(fdm = FDM(Migrants))
  last_yr_migrants <- fit_migrants |>
    forecast(h = 1) |>
    as_tibble() |>
    select(-Migrants, -.model) |>
    rename(Migrants = .mean)
  # Add migrants into last year
  population <- bind_rows(
    df |> filter(Year < last_yr),
    df |> filter(Year == last_yr) |>
      select(-Migrants) |>
      left_join(
        last_yr_migrants,
        by = c("Age", "Year")
      )
  )

  # Forecast mortality rates
  future_death_prob <- mortality |>
    model(fdm = FDM(log(death_prob))) |>
    generate(h = h, times = nsim) |>
    rename(death_prob = .sim) |>
    select(-.model)

  # Simulate future migrant numbers
  future_migrants <- fit_migrants |>
    generate(h = h + 1, times = nsim) |>
    filter(Year > last_yr) |>
    rename(Migrants = .sim) |>
    select(-.model)

  # Simulate future graduate numbers
  future_graduates <- graduates |>
    fit_global_model(arma_coef) |>
    generate(h = h, times = nsim) |>
    rename(Graduates = .sim) |>
    select(-.model)
  # Add actual graduate numbers where available
  future_graduates <- future_graduates |>
    bind_rows(
      graduates |>
        filter(Year >= last_yr) |>
        expand_grid(.rep = unique(future_graduates$.rep))
    )

  # Disaggregate by age
  N <- expand_grid(
    Year = last_yr + seq(h),
    Age = completions$Age,
    .rep = unique(future_graduates$.rep)
  ) |>
    left_join(future_graduates, by = c(".rep", "Year")) |>
    left_join(completions, by = "Age") |>
    mutate(
      pc = if_else(is.na(pc), 0, pc),
      Graduates = pc * Graduates / 100
    ) |>
    select(-pc)

  N <- N |>
    left_join(future_death_prob, by = c("Age", "Year", ".rep")) |>
    left_join(retirements |> select(-pc), by = "Age") |>
    mutate(retire_prob = if_else(Age < 45, 0, retire_prob)) |>
    left_join(future_migrants, by = c("Age", "Year", ".rep")) |>
    filter(Year <= last_yr + h)

  # Get first year working population of simulation
  tmp <- population |>
    filter(Year == last_yr) |>
    transmute(
      Age = Age + 1,
      Year = Year + 1,
      Working = Working - Deaths - Retirees + Graduates + Migrants,
    ) |>
    filter(Age <= 100) |>
    bind_rows(
      tibble(Age = 15, Year = last_yr + 1, Working = 0),
    )

  N <- N |>
    left_join(tmp, by = c("Age", "Year"))

  # Now iterate to generate population each year
  # Pull out year to process
  for (i in 2:h) {
    N1 <- N |>
      filter(Year == last_yr + i) |>
      select(-Working)
    N2 <- N |> filter(Year != last_yr + i)
    # Compute migrants
    next_year <- N |>
      filter(Year == last_yr + i - 1) |>
      transmute(
        .rep = .rep,
        Working = if_else(Age <= 15, 0, Working),
        Year = Year + 1,
        Age = Age + 1
      )
    N1 <- N1 |>
      left_join(next_year, by = c("Age", "Year", ".rep")) |>
      mutate(
        Deaths = Working * death_prob,
        Retirees = Working * retire_prob,
        Retirees = if_else(Retirees < 0, 0, Retirees),
        Working = Working - Deaths - Retirees + Graduates + Migrants,
        Working = if_else(Age == 15, 0, Working),
        Working = if_else(Working < 0, 0, Working)
      ) |>
      filter(Age <= 100) |>
      select(-Deaths, -Retirees)
    N <- bind_rows(N1, N2)
  }
  N |>
    select(Year, Age, .rep, Working) |>
    as_vital(index = Year, key = c(Age, .rep), .age = "Age")
}

# Function to simulate future graduates only
simulate_future_graduates <- function(graduates, arma_coef, h = 18, nsim = 500) {
  future_graduates <- graduates |>
    fit_global_model(arma_coef) |>
    generate(h = h, times = nsim) |>
    rename(Graduates = .sim) |>
    select(-.model)

  last_yr <- max(graduates$Year)
  future_graduates <- future_graduates |>
    bind_rows(
      graduates |>
        filter(Year >= last_yr) |>
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
      mean_grads = mean(Graduates),
      mean_diff = mean(tsibble::difference(Graduates), na.rm=TRUE)
    )

  # Scale course leavers by means and take differences, removing any drift
  scaled_course_leavers <- course_leavers |>
    left_join(mean_grads, by = "discipline") |>
    group_by(discipline) |>
    mutate(Graduates = (tsibble::difference(Graduates) - mean_diff)/ mean_grads)

  # Construct single series of all graduates with large missing sections to wash out memory
  y <- scaled_course_leavers |>
    bind_rows(
      expand_grid(Year = 1960:(min(course_leavers$Year)-1), discipline = unique(course_leavers$discipline))
    ) |>
    arrange(discipline, Year) |>
    pull(Graduates)

  # Fit ARIMA model with d = 0 to differenced data
  global_fit <- tibble(y = y) |>
    mutate(t = row_number()) |>
    as_tsibble(index = t) |>
    model(arima = ARIMA(y ~ 0 + pdq(d = 0)))

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
  if("discipline" %in% colnames(graduates)) {
    tsbl <- graduates |>
      as_tsibble(index = Year, key = discipline)
  } else {
    tsbl <- graduates |>
      as_tsibble(index = Year)
  }
  tsbl |>
    model(arima = ARIMA(Graduates ~ 1 + pdq(p = p, d = 1, q = q),
            fixed = c(coef$estimate, NA), transform.pars = FALSE)
    )
}
