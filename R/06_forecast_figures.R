
make_pop_future_fig_discipline <- function(
  yrs,
  object,
  data,
  no_other = TRUE,
  ymax,
  color_data = FALSE,
  guides = TRUE,
  list = FALSE
) {
  # Remove other group
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    data <- data |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  object <- object |>
    as_tibble() |>
    dplyr::filter(year %in% yrs) |>
    group_by(age, discipline, year) |>
    summarise(
      .lower = quantile(working, probs = 0.1),
      .upper = quantile(working, probs = 0.9),
      working = mean(working),
      .groups = "drop"
    )
  if (list) {
    return(
      lapply(
        yrs,
        make_pop_future_fig_discipline_year,
        object = object,
        data = data,
        ymax = ymax,
        color_data = color_data,
        guides = guides
      )
    )
  }
  p <- ggplot(data) +
    aes(x = age, y = working, group = factor(year)) +
    facet_wrap(~discipline, scales = "free_y")
  if (color_data) {
    p <- p +
      geom_line(aes(color = year)) +
      scale_color_gradientn(colors = rainbow(10))
  } else {
    p <- p + geom_line(color = "gray")
  }
  if (NROW(object) > 0) {
    p <- p +
      geom_ribbon(
        data = object,
        aes(ymin = .lower, ymax = .upper, fill = factor(year)),
        alpha = 0.3
      ) +
      geom_line(
        data = object,
        aes(color = factor(year)),
        linewidth = 0.75
      )
  }
  p <- p +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    labs(
      x = "Age",
      y = "Number of active scientists",
      title = "Forecast of working population by discipline"
    )
  if (!guides) {
    p <- p + guides(color = "none", fill = "none")
  }
  p
}

make_pop_future_fig_discipline_year <- function(
  yrs,
  object,
  data,
  ymax,
  color_data,
  guides = TRUE
) {
  disciplines <- unique(data$discipline)
  object <- object |>
    dplyr::filter(year %in% yrs)
  p <- lapply(disciplines, function(d) {
    tmp <- data |>
      filter(discipline == d) |>
      ggplot() +
      aes(x = age, y = working, group = factor(year)) +
      facet_wrap(~discipline, scales = "free_y")
    if (color_data) {
      tmp <- tmp +
        geom_line(aes(color = year)) +
        scale_color_gradientn(colors = rainbow(10))
    } else {
      tmp <- tmp + geom_line(color = "gray")
    }
    if (NROW(object) > 0) {
      tmp <- tmp +
        geom_ribbon(
          data = object |> filter(discipline == d),
          aes(ymin = .lower, ymax = .upper, fill = factor(year)),
          alpha = 0.3
        ) +
        geom_line(
          data = object |> filter(discipline == d),
          aes(color = factor(year)),
          linewidth = 0.75
        ) +
        guides(color = "none")
    }
    tmp <- tmp +
      scale_x_continuous(breaks = seq(20, 100, by = 10)) +
      labs(
        x = "Age",
        y = "Number of active scientists",
        fill = "Forecast year"
      ) +
      ylim(0, ymax |> filter(discipline == d) |> pull(ymax))
    if (!guides) {
      tmp <- tmp + guides(fill = "none")
    }
    tmp
  })
  if (guides) {
    p[[length(p) + 1]] <- patchwork::guide_area()
  }
  out <- patchwork::wrap_plots(p) +
    patchwork::plot_annotation(
      title = "Forecast of working population by discipline",
    ) +
    patchwork::plot_layout(axis_titles = "collect")
  if (guides) {
    out <- out + patchwork::plot_layout(guides = "collect")
  }
  out
}

make_fig_future_grads_discipline <- function(
  course_leavers,
  future_grads_discipline,
  no_other = TRUE
) {
  if (no_other) {
    course_leavers <- course_leavers |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    future_grads_discipline <- future_grads_discipline |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  future_grads_discipline_pi <- future_grads_discipline |>
    as_tibble() |>
    group_by(discipline, year) |>
    summarise(
      lo = quantile(graduates, prob = 0.05),
      mean = mean(graduates),
      hi = quantile(graduates, prob = 0.95),
      .groups = "drop"
    )
  course_leavers |>
    ggplot(aes(x = year)) +
    facet_wrap(~discipline, scales = "free_y") +
    geom_line(aes(y = graduates)) +
    geom_ribbon(
      data = future_grads_discipline_pi,
      aes(x = year, ymin = lo, ymax = hi),
      alpha = 0.3,
      fill = "#c14b14"
    ) +
    geom_line(
      data = future_grads_discipline_pi,
      aes(x = year, y = mean),
      color = "#c14b14"
    ) +
    labs(
      x = "Year",
      y = "Number of graduates",
      title = "Graduates by discipline"
    ) +
    guides(color = "none")
}

make_future_Ext_fig2 <- function(object, data, h, times, yr, no_other = TRUE) {
  object <- object |>
    forecast(h = h) |>
    filter(year == yr, age <= 100) |>
    mutate(
      pi = hilo(remainder),
      lo = pi$lower,
      hi = pi$upper
    )
  if (no_other) {
    object <- object |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    data <- data |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  if (NROW(object) == 0) {
    title <- "Remainder"
  } else {
    title <- paste("Forecasts of remainder by discipline:", yr)
  }

  data |>
    filter(age <= 100, !is.na(remainder)) |>
    ggplot(aes(x = age)) +
    facet_wrap(~discipline, scales = "free_y") +
    geom_line(
      aes(y = remainder, group = year),
      color = "grey"
    ) +
    geom_ribbon(
      data = object,
      aes(ymin = lo, ymax = hi),
      alpha = 0.3,
      fill = "#c14b14"
    ) +
    geom_line(data = object, aes(y = .mean), color = "#c14b14") +
    labs(
      x = "Age",
      y = latex2exp::TeX("Number of people"), # ($m_{x,t}$)"),
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10))
}

make_fig24 <- function(
  disciplines_combined,
  future_disciplines_combined,
  no_other = TRUE
) {
  if (no_other) {
    disciplines_combined <- disciplines_combined |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    future_disciplines_combined <- future_disciplines_combined |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  sum_disciplines_combined <- disciplines_combined |>
    as_tibble() |>
    group_by(discipline, year) |>
    summarise(working = sum(working), .groups = "drop")

  sum_future_disciplines_combined <- future_disciplines_combined |>
    as_tibble() |>
    group_by(discipline, year, .rep) |>
    summarise(working = sum(working), .groups = "drop") |>
    group_by(discipline, year) |>
    summarise(
      mean = mean(working),
      lo = quantile(working, prob = 0.05),
      hi = quantile(working, prob = 0.95),
      .groups = "drop"
    )

  ggplot(sum_future_disciplines_combined) +
    aes(x = year) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = sum_disciplines_combined, aes(y = working)) +
    labs(
      y = "Total number of working scientists (thousands)",
      title = "Forecast of total working population by discipline",
      #subtitle = "Natural and Physical Sciences\nForecasted years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 10)) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    facet_wrap(~discipline, scales = "free_y")
}
