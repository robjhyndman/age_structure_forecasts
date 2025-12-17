# Script to estimate for each year, the age distribution
# of people graduating with a bachelor degree or higher

read_completions <- function(file) {
  # Read in data for 2006 - 2022
  completions <- list()
  for (i in seq_along(2006:2022)) {
    # Read relevant sheet from xlsx file
    tmp <- readxl::read_excel(
      file,
      sheet = paste(i + 2005),
      skip = 2,
      col_types = "text"
    ) |>
      janitor::clean_names()
    colnames(tmp)[1] <- "age_group"
    # Remove rows that don't contain useful data
    tmp <- tmp |>
      filter(!str_detect(age_group, "% change")) |>
      filter(!str_detect(age_group, "Unknown")) |>
      filter(!str_detect(age_group, "Not provided")) |>
      filter(!str_detect(age_group, "not published"))
    # Convert to long form and add year
    completions[[i]] <- tmp |>
      pivot_longer(
        -age_group,
        names_to = "qualification",
        values_to = "persons"
      ) |>
      mutate(year = 2005L + i)
  }
  completions <- bind_rows(completions)

  # Extract annual totals of bachelor and above completions
  totals <- completions |>
    filter(
      str_detect(age_group, "^(?i)total$"),
      str_detect(qualification, "^(?i)total$") |
        str_detect(qualification, "associate") |
        str_detect(qualification, "other_undergraduate")
    ) |>
    mutate(
      persons = readr::parse_number(persons),
      total = (qualification == "total")
    ) |>
    select(-qualification) |>
    group_by(year, total) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    group_by(year) |>
    summarise(total = abs(diff(persons)), .groups = "drop")

  # To get age group totals, it is better to subtract sub-bachelor from total
  # than to sum over the other degrees as there are too many missing values.
  # Remove uninformative rows and columns
  completions <- completions |>
    filter(!str_detect(age_group, "(?i)total")) |>
    filter(!str_detect(qualification, "not_provided")) |>
    filter(!str_detect(qualification, "sub_total")) |>
    filter(
      str_detect(qualification, "total") |
        str_detect(qualification, "associate") |
        str_detect(qualification, "other_undergraduate")
    )
  # Convert persons to numeric values and compute proportions
  completions <- completions |>
    mutate(
      persons = if_else(persons == "< 5", "2.5", persons),
      persons = if_else(persons == "< 10", "5", persons),
      persons = if_else(persons == "np", NA_character_, persons),
      persons = readr::parse_number(persons),
      total = (qualification == "total")
    ) |>
    select(-qualification) |>
    group_by(year, age_group, total) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    group_by(year, age_group) |>
    summarise(persons = abs(diff(persons)), .groups = "drop") |>
    left_join(totals, by = "year") |>
    mutate(proportion = persons / total)

  # Now read data for 2023 which has a different structure
  comp2023 <- readxl::read_excel(
    file,
    sheet = "2023",
    skip = 2,
    col_types = "text"
  ) |>
    janitor::clean_names() |>
    head(18) |>
    select(age_group, sub_bachelor, total) |>
    mutate(
      year = 2023,
      sub_bachelor = stringr::str_replace(sub_bachelor, "np", "NA"),
      sub_bachelor = readr::parse_number(sub_bachelor),
      total = readr::parse_number(total),
      persons = total - sub_bachelor,
      proportion = persons / (376853 - 27270)
    ) |>
    select(year, age_group, proportion)

  # Combine data from 2006-2022 with data from 2023
  completions <- bind_rows(completions, comp2023) |>
    mutate(
      age_group = stringr::str_replace(age_group, " to ", "-"),
      age_group = stringr::str_replace(age_group, " and over", "+"),
      age_group = stringr::str_replace(age_group, "16 and under", "0-16"),
      pc = proportion * 100
    ) |>
    select(year, age_group, pc)

  return(completions)
}

make_completions_step <- function(completions) {
  completions |>
    make_single_age(pc, smooth = FALSE)
}

make_completions_ave <- function(completions, calc_sd = FALSE) {
  if (calc_sd) {
    FUN <- sd
  } else {
    FUN <- mean
  }
  # Average over years
  ave_completions <- completions |>
    group_by(age_group) |>
    summarise(pc = FUN(pc, na.rm = TRUE)) |>
    make_single_age(pc)

  # Weird behaviour above age 70. Replace with linear interpolation
  if (!calc_sd) {
    ave_completions <- ave_completions |>
      mutate(pc = if_else(age >= 69, 0.013 - 0.0000681 * (age - 69), pc))
  }

  return(ave_completions)
}

# Level 4
read_course_leavers <- function(file) {
  out <- readxl::read_excel(file, sheet = "4-digit") |>
    transmute(
      category = `Broad Field of Education`,
      discipline = `Narrow Field of Education`,
      year = as.integer(Year),
      graduates = Sum
    ) |>
    filter(category == "Natural and Physical Sciences") |>
    select(-category)

  # Combine "Other Natural and Physical Sciences" and "Natural and Physical Sciences (n.f.d.)"
  out$discipline <- if_else(
    out$discipline == "Natural and Physical Sciences (n.f.d.)" |
      out$discipline == "Natural and Physical Sciences, nfd",
    "Other Natural and Physical Sciences",
    out$discipline
  )
  out |>
    group_by(year, discipline) |>
    summarise(graduates = sum(graduates), .groups = "drop")
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
