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

get_ymax <- function(
  yrs,
  object,
  no_other = TRUE
) {
  # Remove other group
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  object <- object |>
    as_tibble() |>
    dplyr::filter(year %in% yrs) |>
    group_by(age, discipline, year) |>
    summarise(
      .upper = quantile(working, probs = 0.9),
      .groups = "drop"
    ) |>
    group_by(discipline) |>
    summarise(ymax = max(.upper))
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
    kableExtra::column_spec(2, width = "9cm") |>
    kableExtra::kable_styling(latex_options = "scale_down")
}
