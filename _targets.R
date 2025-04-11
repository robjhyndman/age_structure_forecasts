library(targets)

tar_option_set(packages = c("dplyr", "ggplot2", "vital", "stringr", "tidyr"))
tar_source()

list(
  # Mortality data from HMD
  tar_target(mx_file, here::here("data/Mx_1x1.txt"), format = "file"),
  tar_target(ex_file, here::here("data/Exposures_1x1.txt"), format = "file"),
  tar_target(mortality, read_mortality(mx_file, ex_file)),
  tar_target(aus_death_prob, compute_death_prob(mortality)),

  # Retirement data
  tar_target(science_file4, here::here("data/Natural and physical sciences 4-digit.xlsx")),
  tar_target(retirement_data, read_retirements(science_file4)),
  tar_target(retirements, single_age_retirements(retirement_data, aus_death_prob)),

  # Graduates
  tar_target(grad_file, here::here("data/Award course completions for all students by age group and course level.xlsx")),
  tar_target(completions, read_completions(grad_file)),

  # Course leavers
  tar_target(leavers_file, here::here("data/Course completions - 2006 to 2023 - Totals.xlsx")),
  tar_target(course_leavers, read_course_leavers(leavers_file)),

  NULL
)
