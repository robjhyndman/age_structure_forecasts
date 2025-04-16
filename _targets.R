library(targets)
library(tarchetypes)
tar_option_set(
  packages = c(
    "dplyr",
    "ggplot2",
    "vital",
    "stringr",
    "tidyr",
    "tsibble",
    "fable",
    "feasts",
    "knitr",
    "kableExtra",
    "patchwork"
  ),
  seed = 2025
)
tar_source()

list(
  # Mortality data from HMD
  tar_target(mx_file, here::here("data/Mx_1x1.txt"), format = "file"),
  tar_target(ex_file, here::here("data/Exposures_1x1.txt"), format = "file"),
  tar_target(mortality, read_mortality(mx_file, ex_file)),
  tar_target(aus_death_prob, compute_death_prob(mortality)),
  tar_target(fig8, make_fig8(aus_death_prob)),

  # Retirement data
  tar_target(
    science_file4,
    here::here("data/Natural and physical sciences 4-digit.xlsx")
  ),
  tar_target(retirement_data, read_retirements(science_file4)),
  tar_target(
    retirements,
    single_age_retirements(retirement_data, aus_death_prob)
  ),
  tar_target(fig4, make_fig4(retirement_data)),
  tar_target(fig6, make_fig6(retirements, pc)),
  tar_target(fig7, make_fig6(retirements, retire_prob)),

  # Graduates
  tar_target(
    grad_file,
    here::here(
      "data/Award course completions for all students by age group and course level.xlsx"
    )
  ),
  tar_target(completions, read_completions(grad_file)),
  tar_target(completions_step, make_completions_step(completions)),
  tar_target(ave_completions, make_completions_ave(completions)),
  tar_target(fig9, make_fig9(completions_step)),
  tar_target(fig10, make_fig10(ave_completions)),
  tar_target(tab2, make_table2(ave_completions)),

  # Course leavers
  tar_target(
    leavers_file,
    here::here("data/Course completions - 2006 to 2023 - Totals.xlsx")
  ),
  tar_target(course_leavers, read_course_leavers(leavers_file)),
  tar_target(arma_coef_science, global_arma(course_leavers)),
  tar_target(
    future_course_leavers_science,
    simulate_future_graduates(course_leavers, arma_coef_science) |>
      select(-category)
  ),

  # Census 2 digit
  tar_target(science_file2, here::here("data/2-digit - Single year.xlsx")),
  tar_target(census2, read_census2(science_file2)),
  tar_target(
    census2_1,
    make_census_single_year(
      census2,
      course_leavers,
      ave_completions,
      retirements,
      aus_death_prob
    )
  ),
  tar_target(fig1, make_fig1(census2)),
  tar_target(fig2, make_fig2(census2_1, "Natural and Physical Sciences")),
  tar_target(fig3, make_fig3(census2_1, "Natural and Physical Sciences")),

  # Census 4 digit
  tar_target(census4, read_census(science_file4)),
  tar_target(
    census4_1,
    make_census_single_year(
      census4,
      course_leavers,
      ave_completions,
      retirements,
      aus_death_prob
    )
  ),

  # Forecasts
  tar_target(h, 20),
  tar_target(nsim, 500),
  tar_target(
    future_pop_science,
    forecast_pop(
      census4_1,
      course_leavers,
      ave_completions,
      retirements,
      aus_death_prob,
      arma_coef_science,
      h = h,
      nsim = nsim
    )
  ),

  # # Physics
  tar_target(
    physics,
    census4_1 |>
      filter(discipline == "Physics and Astronomy") |>
      select(-discipline)
  ),
  tar_target(
    future_physics,
    future_pop_science |>
      filter(discipline == "Physics and Astronomy") |>
      select(-discipline)
  ),
  tar_target(fig16, make_fig16(physics, future_physics)),

  # Disciplines combined
  tar_target(fig19, make_fig19(census4_1)),
  tar_target(fig20, make_fig20(census4_1)),
  tar_target(fig21, make_fig21(course_leavers)),
  tar_target(fig22, make_fig22(census4_1)),
  tar_target(fig23, make_fig23(future_pop_science)),
  tar_target(fig24, make_fig24(census4_1, future_pop_science)),
  tar_target(
    fig_grad_forecasts,
    make_fig_grad_forecasts(course_leavers, future_course_leavers_science)
  ),

  # Document
  tar_target(discipline_table, make_discipline_table()),
  tar_quarto(
    report,
    "age_structure_forecasts.qmd",
    output_file = "age_structure_forecasts.pdf"
  )
)
