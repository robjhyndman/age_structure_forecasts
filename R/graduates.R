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

make_completions_ave <- function(completions) {
  # Average over years
  ave_completions <- completions |>
    group_by(age_group) |>
    summarise(pc = mean(pc, na.rm = TRUE)) |>
    make_single_age(pc)

  # Weird behaviour above age 70. Replace with linear interpolation
  ave_completions <- ave_completions |>
    mutate(pc = if_else(age >= 69, 0.013 - 0.0000681 * (age - 69), pc))

  return(ave_completions)
}

make_fig_completions <- function(completions, average = FALSE) {
  p <- ggplot(completions)
  if (average) {
    p <- p +
      aes(x = age, y = pc) +
      labs(title = "Average graduate completions by age (2006 – 2023)")
  } else {
    p <- p +
      aes(x = age, y = pc, colour = year, group = year) +
      scale_color_gradientn(colours = rainbow(10)[1:8]) +
      labs(title = "Graduate completions by year and age (2006 – 2023)")
  }
  p +
    geom_line() +
    labs(
      x = "Age",
      y = "Percentage of graduates",
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
}

make_table2 <- function(ave_completions) {
  ave_completions |>
    filter(age >= 20 & age <= 25) |>
    mutate(pc = paste0(round(pc, 2), "%")) |>
    select(age, pc) |>
    kable(col.names = c("Age", "Percentage of Graduates"), align = "c") |>
    add_header_above(
      c("Percentage of Graduates Ages 20-25" = 2),
      bold = TRUE
    ) |>
    kable_styling(latex_options = c("striped"))
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

# Level 2

total_sci_grads <- function(course_leavers) {
  course_leavers |>
    group_by(year) |>
    summarise(graduates = sum(graduates), .groups = "drop") |>
    as_tsibble(index = year)
}
