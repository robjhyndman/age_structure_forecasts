# Forecast the working population for each discipline
forecast_pop <- function(
  df,
  graduates,
  ave_completions,
  sd_completions,
  retirements,
  death_prob,
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
    make_sd(remainder, key = discipline) |>
    model(fdm = FDM(remainder, coherent = TRUE))

  # Simulate future migrant numbers
  future_migrants <- fit_migrants |>
    generate(h = h + 1, times = nsim) |>
    as_tibble() |>
    rename(remainder = .sim) |>
    select(-.model)
  # Add mean to other disciplines
  fm <- left_join(
    future_migrants |> filter(discipline != "mean"),
    future_migrants |> filter(discipline == "mean"),
    by = c("year", "age", ".rep")
  ) |>
  mutate(
    remainder = remainder.x + remainder.y,
    discipline = discipline.x
  ) |>
  select(year, age, discipline, .rep, remainder) 
  
  last_yr_migrants <- fm |>
    filter(year == min(year))

  # Add migrants into last year
  population <- df |>
    filter(year == last_yr) |>
    select(-remainder) |>
    as_tibble() |>
    left_join(last_yr_migrants, by = c("age", "year", "discipline"))

  # Remove from forecasts
  future_migrants <- fm |>
    filter(year > last_yr)
  
  # Forecast mortality rates
  future_death_prob <- death_prob |>
    model(fdm = FDM(log(qx))) |>
    generate(h = h, times = nsim) |>
    rename(mortality = .sim) |>
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
    left_join(future_graduates, by = c(".rep", "year"), relationship = "many-to-many") |>
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
    left_join(future_migrants, by = c("age", "year", ".rep", "discipline")) |>
    filter(year <= last_yr + h)

  # Get first year working population of simulation
  tmp <- population |>
    transmute(
      discipline = discipline,
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
    left_join(tmp, by = c("discipline", "age", "year", ".rep")) |>
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
    discipline = discipline,
        .rep = .rep,
        working = if_else(age <= 15, 0, working),
        year = year + 1,
        age = age + 1
      )
    N1 <- N1 |>
      left_join(next_year, by = c("discipline", "age", "year", ".rep")) |>
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
    as_vital(index = year, key = c(discipline, age, .rep), .age = "age")
}
