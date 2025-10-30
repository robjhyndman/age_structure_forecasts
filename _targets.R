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
  tar_target(fig_mxt, life_table(mortality) |> make_fig_mxt()),
  tar_target(
    fig_smooth_mxt,
    make_fig_mxt(aus_death_prob) +
      labs(y = "Smoothed probability of death")
  ),
  tar_target(
    model_mxt,
    aus_death_prob |>
      model(fdm = FDM(log(qx)))
  ),
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
  tar_target(fig_r, make_fig_r(retirement_data)),
  tar_target(fig_rx1, make_fig_rx(retirements, retirement_data, FALSE)),
  tar_target(fig_rx2, make_fig_rx(retirements, retirement_data)),

  # Graduates
  tar_target(
    leavers_file,
    here::here("data/Course completions - 2006 to 2023 - Totals.xlsx")
  ),
  tar_target(course_leavers, read_course_leavers(leavers_file)),
  tar_target(arma_coef_science, global_arma(course_leavers)),
  tar_target(
    future_course_leavers_science,
    simulate_future_graduates(course_leavers, arma_coef_science)
  ),

  # Graduates by age
  tar_target(
    grad_file,
    here::here(
      "data/Award course completions for all students by age group and course level.xlsx"
    )
  ),
  tar_target(completions, read_completions(grad_file)),
  tar_target(completions_step, make_completions_step(completions)),
  tar_target(ave_completions, make_completions_ave(completions)),
  tar_target(fig_completions, make_fig_completions(completions_step)),
  tar_target(
    fig_ave_completions,
    make_fig_completions(
      completions_step,
      ave_completions,
      average = TRUE,
      by_year = TRUE
    )
  ),
  tar_target(tab2, make_table2(ave_completions)),

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
  tar_target(
    fig_Pxt_census,
    make_pop_fig(census2_1, "Natural and Physical Sciences", FALSE, TRUE)
  ),
  tar_target(
    fig_Pxt,
    make_pop_fig(census2_1, "Natural and Physical Sciences", TRUE, TRUE)
  ),
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

  tar_target(fig_error, make_component_fig(census2_1, remainder)),

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

  # Remainders
  tar_target(
    model_Ext,
    census2_1 |>
      filter(year <= 2020) |>
      model(fdm = FDM(remainder, coherent = TRUE))
  ),
  tar_target(
    fig_Emodel_fdm1,
    make_future_fdm_fig(model_Ext, 1:2)
  ),
  tar_target(
    fig_Emodel_fdm2,
    make_future_fdm_fig(model_Ext, 3:4)
  ),
  tar_target(
    fig_Emodel_fdm3,
    make_future_fdm_fig(model_Ext, 5:6)
  ),
  tar_target(
    fig_model_Ext0,
    make_future_Ext_fig(model_Ext, census2_1, h, 10, 2050)
  ),
  tar_target(
    fig_model_Ext1,
    make_future_Ext_fig(model_Ext, census2_1, h, 10, 2030)
  ),
  tar_target(
    fig_model_Ext2,
    make_future_Ext_fig(model_Ext, census2_1, h, 10, 2040)
  ),
  # By discipline
  tar_target(
    model_Ext_discipline,
    census4_1 |>
      filter(year <= 2020) |>
      model(fdm = FDM(remainder, coherent = TRUE))
  ),
  tar_target(
    fig_model_Ext3,
    make_future_Ext_fig2(model_Ext_discipline, census4_1, h, 10, 2035)
  ),

  # Forecasts
  tar_target(h, 20),
  tar_target(nsim, 1000),
  tar_target(
    future_pop_science2,
    forecast_pop2(
      census2_1,
      sci_grads,
      ave_completions,
      retirements,
      aus_death_prob,
      arma_coef_science,
      h = h,
      nsim = nsim
    )
  ),
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
  tar_target(
    fig_Pxt0,
    make_pop_future_fig(future_pop_science2, census2_1, 2050)
  ),
  tar_target(
    fig_Pxt_future,
    make_pop_future_fig(
      future_pop_science2,
      census2_1,
      2022:2035,
      ribbon = TRUE
    )
  ),
  # Physics
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
  tar_target(
    fig19,
    make_pop_fig(census4_1, "Natural and Physical Sciences", FALSE, TRUE)
  ),
  tar_target(
    fig20,
    make_pop_fig(census4_1, "Natural and Physical Sciences", TRUE, TRUE)
  ),
  tar_target(fig21, make_fig21(course_leavers)),
  tar_target(fig21b, make_fig21(course_leavers, combine = TRUE)),
  tar_target(fig22, make_fig22(census4_1)),
  tar_target(ymax, get_ymax(2022:2035, future_pop_science)),
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

  # Graduate forecasts
  tar_target(sci_grads, total_sci_grads(course_leavers)),
  tar_target(arima, sci_grads |> model(ARIMA(graduates))),
  tar_target(
    check_arima,
    if (fabletools::model_sum(arima[[1]][[1]]) != "ARIMA(0,1,1) w/ drift") {
      stop("Wrong model")
    }
  ),
  tar_target(
    future_grads,
    generate(arima, h = h, times = nsim) |>
      rename(graduates = .sim) |>
      select(-.model)
  ),
  tar_target(
    fig_future_grads,
    sci_grads |>
      ggplot(aes(x = year, y = graduates)) +
      geom_line() +
      geom_line(
        # Only show first 9 sample paths
        data = future_grads |> filter(stringr::str_length(.rep) < 2),
        aes(color = .rep)
      ) +
      labs(
        x = "Year",
        y = "Number of graduates",
        title = "Total Science Graduates: Australia"
      ) +
      guides(color = "none")
  ),

  # Graduate forecasts by discipline
  tar_target(
    future_grads_discipline,
    course_leavers |>
      fit_global_model(arma_coef_science) |>
      generate(h = h, times = nsim) |>
      rename(graduates = .sim) |>
      select(-.model)
  ),
  tar_target(
    fig_future_grads_discipline,
    make_fig_future_grads_discipline(course_leavers, future_grads_discipline)
  ),

  # Document
  tar_target(discipline_table, make_discipline_table()),
  # tar_quarto(
  #   paper,
  #   "age_structure_forecasts.qmd",
  #   extra_files = c("refs.bib", "preamble.tex"),
  #   quiet = FALSE
  # ),
  tar_quarto(
    talk,
    "age_structure_talk.qmd",
    output_file = "age_structure_talk.pdf",
    extra_files = c("setup.R", "header.tex", "before-title.tex"),
    quiet = FALSE
  )
)
