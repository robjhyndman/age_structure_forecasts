# Read mortality data from HMD

read_mortality <- function(mx_path, ex_path) {
  read_hmd_files(c(mx_path, ex_path)) |>
    filter(Sex == "Total", Year >= 1970) |>
    collapse_ages(max_age = 101) |>
    suppressWarnings() |>
    select(
      year = Year,
      age = Age,
      mortality = Mortality,
      exposures = Exposures
    ) |>
    as_vital(index = year, key = age, .age = "age", population = "exposures")
}

# Compute lifetable to convert mortality rates to probabilities
# Then smooth the result and add in population data
compute_death_prob <- function(mortality) {
  life_table(mortality) |>
    smooth_mortality(qx, k = 60) |>
    mutate(qx = c(.smooth)) |>
    select(year, age, qx) |>
    left_join(
      mortality |> select(year, age, population = exposures),
      by = c("year", "age")
    ) |>
    as_vital(index = year, key = age, .age = "age", populaton = "exposures")
}


# Graph showing the probability of death by age over time
make_fig_mxt <- function(death_prob) {
  death_prob |>
    filter(age <= 100) |>
    autoplot(qx) +
    labs(
      x = "Age",
      y = latex2exp::TeX("Probability of death"), # ($m_{x,t}$)"),
      title = "Probability of death for Australians (1971 – 2021)",
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_log10(labels = scales::label_number(), limits = c(2e-5, 1))
}

make_future_mxt_fig <- function(object, data, h, times, yr) {
  object |>
    generate(h = h, times = times) |>
    filter(year == yr, age <= 100) |>
    ggplot() +
    geom_line(
      data = life_table(data) |> filter(age <= 100),
      aes(x = age, y = qx, group = year),
      color = "grey"
    ) +
    geom_line(aes(x = age, y = .sim, col = .rep, group = .rep)) +
    guides(color = "none") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Probability of death"), # ($m_{x,t}$)"),
      title = paste("Simulated future mortality for", yr)
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_log10(labels = scales::label_number(), limits = c(2e-5, 1))
}

# Adaptation of vital:::autoplot.FDM to add in future sample paths
make_future_fdm_fig <- function(object, show_order = 1:2) {
  obj_time <- time_components(object)
  obj_x <- age_components(object)

  meanvar <- "mean"
  tmp <- colnames(obj_time)
  timevar <- tmp[grepl("beta", tmp)]
  tmp <- colnames(obj_x)
  agevar <- tmp[grepl("phi", tmp)]
  index <- tsibble::index_var(obj_time)
  keys <- head(colnames(object), -1)

  # Set up list of plots
  p <- list()
  p[[1]] <- vital:::age_plot(obj_x, meanvar, keys) +
    labs(x = "Age (x)", y = "")
  for (i in show_order) {
    p[[length(p) + 1]] <- vital:::age_plot(obj_x, agevar[i], keys) +
      labs(x = "Age (x)", y = "")
  }
  p[[length(p) + 1]] <- patchwork::guide_area()
  for (i in show_order) {
    p[[length(p) + 1]] <- vital:::time_plot(obj_time, timevar[i], keys)
    df <- obj_time[, c(index, timevar[i], keys)]
    colnames(df)[2] <- "y"
    fit <- df |> model(ARIMA(y))
    future <- fit |> generate(h = 20, times = 10)
    p[[length(p)]] <- p[[length(p)]] +
      geom_line(data = future, aes(col = .rep, y = .sim)) +
      labs(x = "Year (t)", y = "") +
      guides(color = "none")
  }
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(
      ncol = length(show_order) + 1,
      nrow = 2,
      guides = "collect"
    )
}
