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
  # Mortality data
  tar_target(mx_file, here::here("data/Mx_1x1.txt"), format = "file"),
  tar_target(ex_file, here::here("data/Exposures_1x1.txt"), format = "file"),
  tar_target(mortality, read_mortality(mx_file, ex_file)),
  tar_target(aus_death_prob, compute_death_prob(mortality)),
  tar_target(
    model_mxt,
    aus_death_prob |>
      model(fdm = FDM(log(qx)))
  ),

  # Mortality figures
  tar_target(fig_mxt, life_table(mortality) |> make_fig_mxt()),
  tar_target(
    fig_model_fdm1,
    make_future_fdm_fig(model_mxt, 1:2)
  ),
  tar_target(
    fig_model_fdm2,
    make_future_fdm_fig(model_mxt, 3:4)
  ),
  tar_target(
    fig_model_fdm3,
    make_future_fdm_fig(model_mxt, 5:6)
  ),
  tar_target(
    fig_model_mxt0,
    make_future_mxt_fig(model_mxt, mortality, h, 10, 2050)
  ),
  tar_target(
    fig_model_mxt1,
    make_future_mxt_fig(model_mxt, mortality, h, 10, 2030)
  ),
  tar_target(
    fig_model_mxt2,
    make_future_mxt_fig(model_mxt, mortality, h, 10, 2040)
  ),
  tar_target(
    fig_model_mxt3,
    make_future_mxt_fig2(model_mxt, mortality, h, 10, 2035)
  ),

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

  # Retirement figures
  tar_target(fig_r, make_fig_r(retirement_data)),
  tar_target(fig_rx1, make_fig_rx(retirements, retirement_data, FALSE)),
  tar_target(fig_rx2, make_fig_rx(retirements, retirement_data)),

  # Total graduates by discipline
  tar_target(
    leavers_file,
    here::here("data/Course completions - 2006 to 2023 - Totals.xlsx")
  ),
  tar_target(course_leavers, read_course_leavers(leavers_file)),

  # Graduate model and forecasts
  tar_target(arma_coef_science, global_arma(course_leavers)),
  tar_target(
    future_course_leavers_science,
    simulate_future_graduates(course_leavers, arma_coef_science)
  ),

  # Graduate figures
  tar_target(
    fig_grad_forecasts,
    make_fig_grad_forecasts(course_leavers, future_course_leavers_science)
  ),
  tar_target(
    fig_grad_sim,
    make_fig_grad_forecasts(
      course_leavers,
      future_course_leavers_science,
      PI = FALSE
    )
  ),

  # Total graduates by age
  tar_target(
    grad_file,
    here::here(
      "data/Award course completions for all students by age group and course level.xlsx"
    )
  ),
  tar_target(completions, read_completions(grad_file)),
  tar_target(completions_step, make_completions_step(completions)),
  tar_target(ave_completions, make_completions_ave(completions)),
  tar_target(sd_completions, make_completions_ave(completions, calc_sd = TRUE)),

  # Graduate by age figures
  tar_target(fig_completions, make_fig_completions(completions_step)),
  tar_target(
    fig_ave_completions,
    make_fig_completions(
      completions_step,
      ave_completions,
      sd_completions,
      average = TRUE,
      by_year = TRUE
    )
  ),
  tar_target(
    fig_ave_completions2,
    make_fig_completions(
      completions_step,
      ave_completions,
      sd_completions,
      average = TRUE,
      by_year = TRUE,
      pi = TRUE
    )
  ),

  # Census data (4 digit level for disciplines)
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

  # Census figures
  tar_target(
    fig19,
    make_pop_fig(census4_1, "Natural and Physical Sciences", FALSE, TRUE)
  ),
  tar_target(
    fig20,
    make_pop_fig(census4_1, "Natural and Physical Sciences", TRUE, TRUE)
  ),
  tar_target(fig21, make_fig21(course_leavers)),
  tar_target(fig22, make_fig22(census4_1)),

  # Remainder model
  tar_target(
    model_Ext_discipline,
    census4_1 |>
      filter(year <= 2020) |>
      make_sd(remainder, key = discipline) |>
      model(fdm = FDM(remainder, coherent = TRUE))
  ),

  # Remainder figures
  tar_target(
    fig_model_Ext3,
    make_future_Ext_fig2(model_Ext_discipline, census4_1, h, 10, 2035)
  ),

  # Population forecasts
  tar_target(h, 20),
  tar_target(nsim, 1000),
  tar_target(
    future_pop_science,
    forecast_pop(
      census4_1,
      course_leavers,
      ave_completions,
      sd_completions,
      retirements,
      aus_death_prob,
      arma_coef_science,
      h = h,
      nsim = nsim
    )
  ),
  tar_target(ymax, get_ymax(2022:2035, future_pop_science)),

  # Forecast figures
  tar_target(
    fig_Pxt_discipline,
    #make_pop_fig(census4_1, "Natural and Physical Sciences", TRUE, TRUE)
    make_pop_future_fig_discipline(
      2050,
      future_pop_science,
      census4_1,
      no_other = TRUE,
      color_data = TRUE,
      ymax = ymax,
      list = TRUE
    )[[1]]
  ),
  tar_target(
    fig_Pxt_discipline0,
    make_pop_future_fig_discipline(
      2050,
      future_pop_science,
      census4_1,
      ymax = ymax,
      list = TRUE
    )[[1]]
  ),
  tar_target(
    fig_Pxt_future_discipline,
    make_pop_future_fig_discipline(
      2035,
      future_pop_science,
      census4_1,
      ymax = ymax,
      guides = FALSE
    )
  ),
  tar_target(
    fig_Pxt_future_discipline_talk,
    make_pop_future_fig_discipline(
      2022:2035,
      future_pop_science,
      census4_1,
      ymax = ymax,
      list = TRUE
    )
  ),
  tar_target(fig24, make_fig24(census4_1, future_pop_science)),

  # Appendix
  tar_target(
    future_pop_science_2016,
    forecast_pop(
      census4_1 |> filter(year <= 2016),
      course_leavers |> filter(year <= 2016),
      ave_completions,
      sd_completions,
      retirements,
      aus_death_prob |> filter(year <= 2016),
      arma_coef_science,
      h = 5,
      nsim = nsim
    )
  ),
  tar_target(
    forecast_error,
    future_pop_science_2016 |>
      as_tibble() |>
      filter(
        year == 2021,
        !stringr::str_detect(discipline, "Other")
      ) |>
      summarise(forecast = mean(working), .by = c(age, discipline)) |>
      left_join(
        census4_1 |>
          as_tibble() |>
          filter(year == 2021) |>
          select(age, discipline, working),
        by = c("age", "discipline")
      ) |>
      mutate(error = working - forecast)
  ),
  tar_target(
    fe_20_70,
    forecast_error |>
      filter(age >= 20, age <= 70)
  ),
  tar_target(
    mdape,
    fe_20_70 |>
      summarise(
        mdape = median(100 * abs(error / working), na.rm = TRUE),
        .by = discipline
      ) |>
      bind_rows(
        tibble(
          discipline = "Overall",
          mdape = median(
            100 * abs(fe_20_70$error / fe_20_70$working),
            na.rm = TRUE
          )
        )
      )
  ),
  tar_target(
    fc_2021_plot,
    plot_forecast_2021(forecast_error)
  ),
  tar_target(
    coverage,
    future_pop_science_2016 |>
      filter(year == 2021) |>
      group_by(age, discipline) |>
      reframe(quantile_df(working)) |>
      left_join(
        census4_1 |>
          filter(year == 2021) |>
          select(age, discipline, working),
        by = c("age", "discipline")
      ) |>
      group_by(discipline, interval) |>
      summarise(
        inside = mean(working >= lo & working <= hi)
      )
  ),

  # Document
  tar_target(discipline_table, make_discipline_table()),
  tar_quarto(
    paper,
    "age_structure_forecasts.qmd",
    extra_files = c("refs.bib", "preamble.tex"),
    quiet = FALSE
  ),
  tar_quarto(
    response2,
    "response2.qmd",
    extra_files = "refs.bib",
    quiet = FALSE
  ),
  tar_quarto(
    talk,
    "age_structure_talk.qmd",
    output_file = "age_structure_talk.pdf",
    extra_files = c("setup.R", "header.tex", "before-title.tex"),
    quiet = FALSE
  )
)
