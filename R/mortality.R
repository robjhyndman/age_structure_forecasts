# Read mortality data from HMD

read_mortality <- function(mx_path, ex_path) {
  read_hmd_files(c(mx_path, ex_path)) |>
    filter(Sex == "Total", Year >= 1970) |>
    collapse_ages() |>
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
    smooth_mortality(qx, k = 50) |>
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
    autoplot(qx) +
    scale_y_log10(labels = scales::label_number()) +
    labs(
      y = latex2exp::TeX("Probability of death"), # ($m_{x,t}$)"),
      x = "Age",
      title = "Probability of death for Australians (1971 – 2021)",
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}

# Adaptation of vital:::autoplot.FDM to add in future sample paths
make_future_mxt_fig <- function(object, show_order = 2) {
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
  p[[1]] <- vital:::age_plot(obj_x, meanvar, keys) + ggplot2::ylab(meanvar)
  for (i in seq(show_order)) {
    p[[i + 1]] <- vital:::age_plot(obj_x, agevar[i], keys)
  }
  p[[show_order + 2]] <- patchwork::guide_area()
  for (i in seq(show_order)) {
    p[[i + 2 + show_order]] <- vital:::time_plot(obj_time, timevar[i], keys)
    df <- obj_time[, c(index, timevar[i], keys)]
    colnames(df)[2] <- "y"
    fit <- df |> model(ARIMA(y))
    future <- fit |> generate(h = 20, times = 10)
    p[[i + 2 + show_order]] <- p[[i + 2 + show_order]] +
      geom_line(data = future, aes(col = .rep, y = .sim)) +
      guides(color = "none")
  }
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = show_order + 1, nrow = 2, guides = "collect")
}
