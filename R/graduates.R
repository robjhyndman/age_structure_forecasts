# Script to estimate for each year, the age distribution
# of people graduating with a bachelor degree or higher

read_completions <- function(file) {
  # Read in data for 2006 - 2022
  completions <- list()
  for (i in seq_along(2006:2022)) {
    # Read relevant sheet from xlsx file
    tmp <- readxl::read_excel(file,
      sheet = paste(i + 2005), skip = 2, col_types = "text"
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
      pivot_longer(-age_group, names_to = "qualification", values_to = "persons") |>
      mutate(Year = 2005L + i)
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
    group_by(Year, total) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    group_by(Year) |>
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
    group_by(Year, age_group, total) |>
    summarise(persons = sum(persons), .groups = "drop") |>
    group_by(Year, age_group) |>
    summarise(persons = abs(diff(persons)), .groups = "drop") |>
    left_join(totals, by = "Year") |>
    mutate(proportion = persons / total)

  # Now read data for 2023 which has a different structure
  comp2023 <- readxl::read_excel(file, sheet = "2023", skip = 2, col_types = "text") |>
    janitor::clean_names() |>
    head(18) |>
    select(age_group, sub_bachelor, total) |>
    mutate(
      Year = 2023,
      sub_bachelor = stringr::str_replace(sub_bachelor, "np", "NA"),
      sub_bachelor = readr::parse_number(sub_bachelor),
      total = readr::parse_number(total),
      persons = total - sub_bachelor,
      proportion = persons / (376853 - 27270)
    ) |>
    select(Year, age_group, proportion)

  # Combine data from 2006-2022 with data from 2023
  completions <- bind_rows(completions, comp2023) |>
    mutate(
      age_group = stringr::str_replace(age_group, " to ", "-"),
      age_group = stringr::str_replace(age_group, " and over", "+"),
      age_group = stringr::str_replace(age_group, "16 and under", "0-16"),
      pc = proportion * 100
    ) |>
    select(Year, age_group, pc)

  return(completions)
}

make_completions_step <- function(completions) {
  completions |>
    make_single_age(pc, smooth = FALSE)
}

make_completions_ave <- function(completions) {
  # Average over years
  ave_completions <- completions |>
    group_by(age_group) |>
    summarise(pc = mean(pc, na.rm = TRUE)) |>
    make_single_age(pc)

  # Weird behaviour above age 70. Replace with linear interpolation
  ave_completions <- ave_completions |>
    mutate(pc = if_else(Age >= 69, 0.013 - 0.0000681 * (Age - 69), pc))

  return(ave_completions)
}
