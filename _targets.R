library(targets)

tar_option_set(packages = c("dplyr", "ggplot2", "vital"))
tar_source()

list(
  # Mortality data from HMD
  tar_target(mx_file, here::here("data/Mx_1x1.txt"), format = "file"),
  tar_target(ex_file, here::here("data/Exposures_1x1.txt"), format = "file"),
  tar_target(mortality, read_mortality(mx_file, ex_file)),
  tar_target(aus_death_prob, compute_death_prob(mortality)),

  # Industry employment data
  tar_target(industry_file, here::here("data/Industry.xlsx"), format = "file"),
  tar_target(industry, read_industry(industry_file)),
  tar_target(industry_summary, summarise_industry(industry)),
  tar_target(top_industries, find_top_industries(industry_summary))
)
