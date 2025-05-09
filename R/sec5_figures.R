make_fig19 <- function(disciplines_combined) {
  years <- seq(2006, 2021, by = 5)
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF")
  names(cols) <- years

  disciplines_combined |>
    as_tibble() |>
    filter(year %in% years) |>
    mutate(Year = factor(year, levels = rev(years))) |>
    ggplot() +
    aes(x = age, y = working, color = Year, group = Year) +
    geom_line() +
    scale_color_manual(values = cols, name = "Census Year") +
    labs(
      y = "Number of active scientists",
      title = "Working population by discipline",
      subtitle = "Natural and Physical Sciences"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    facet_wrap(~discipline, scales = "free_y") +
    theme(
      legend.position = "top"
    )
}

make_fig20 <- function(disciplines_combined) {
  ggplot(disciplines_combined) +
    aes(x = age, y = working, group = year, color = year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Number of active scientists",
      title = "Interpolated working population by discipline",
      subtitle = "Natural and Physical Sciences\n2006 – 2021"
    ) +
    theme(
      legend.position = "top"
    )
}

make_fig21 <- function(course_leavers, combine = FALSE, no_other = TRUE) {
  if(no_other) {
    course_leavers <- course_leavers |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  subtitle <- "(2006 – 2023)"
  if (combine) {
    course_leavers <- course_leavers |>
      group_by(year) |>
      summarise(graduates = sum(graduates), .groups = "drop")
  }

  p <- ggplot(course_leavers) +
    aes(x = year, y = graduates) +
    geom_line() +
    labs(
      x = "Year",
      y = "Number of graduates",
    ) +
    theme(legend.position = "none")

  if (!combine) {
    p <- p +
      facet_wrap(~discipline, scales = "free_y") +
      labs(title = paste("Graduates by discipline:", subtitle))
  } else {
    p <- p +
      labs(title = paste("Total graduates:", subtitle))
  }
  p
}

make_fig22 <- function(disciplines_combined) {
  ggplot(disciplines_combined) +
    aes(x = age, y = remainder, group = year, color = year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(10, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Remainder",
      title = "Remainder by discipline",
      subtitle = "Natural and Physical Sciences (2006 - 2020)"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    theme(
      legend.position = "top"
    )
}

make_fig24 <- function(disciplines_combined, future_disciplines_combined, no_other = TRUE) {
  if(no_other) {
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
      subtitle = "Natural and Physical Sciences\nForecasted years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 10)) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    facet_wrap(~discipline, scales = "free_y")
}

make_fig_grad_forecasts <- function(grads, future_grads, no_other = TRUE) {
  if(no_other) {
    grads <- grads |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    future_grads <- future_grads |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  future_grads <- future_grads |>
    group_by(discipline) |>
    summarise(
      mean = mean(graduates),
      lo = quantile(graduates, prob = 0.05),
      hi = quantile(graduates, prob = 0.95)
    )
  grads <- grads |>
    as_tibble() |>
    group_by(discipline, year) |>
    summarise(graduates = sum(graduates))
  future_grads |>
    as_tibble() |>
    ggplot() +
    aes(x = year, group = discipline) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = grads, aes(y = graduates)) +
    labs(
      y = "Number of graduates",
      title = "Forecasts of total graduates by discipline: 2024–2041",
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    facet_wrap(~discipline, scales = "free_y")
}
