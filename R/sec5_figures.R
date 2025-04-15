make_fig19 <- function(disciplines_combined) {
  years <- seq(2006, 2021, by = 5)
  cols <- c("#FF6F00", "#009E73", "#009DFF", "#FF00BF")
  names(cols) <- years

  disciplines_combined |>
    as_tibble() |>
    filter(Year %in% years) |>
    mutate(Year = factor(Year, levels = rev(years))) |>
    ggplot() +
    aes(x = Age, y = Working, color = Year, group = Year) +
    geom_line() +
    scale_color_manual(values = cols, name = "Census Year") +
    labs(
      y = "Number of active scientists",
      title = "Working Population by Discipline",
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
    aes(x = Age, y = Working, group = Year, color = Year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Number of active scientists",
      title = "Interpolated Working Population by Discipline",
      subtitle = "Natural and Physical Sciences\n2006 - 2021"
    ) +
    theme(
      legend.position = "top"
    )
}

make_fig21 <- function(course_leavers) {
  course_leavers$discipline <- course_leavers$discipline |>
    forcats::fct_recode(
      "Natural and Physical Sciences (n.f.d.)" = "Natural and Physical Sciences, nfd"
    ) |>
    forcats::fct_relevel(
      "Physics and Astronomy",
      "Mathematical Sciences",
      "Chemical Sciences",
      "Earth Sciences",
      "Biological Sciences",
      "Other Natural and Physical Sciences",
      "Natural and Physical Sciences (n.f.d.)"
    )

  ggplot(course_leavers) +
    aes(x = Year, y = Graduates) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    labs(
      y = "Number of graduates",
      title = "Graduates by Discipline",
      subtitle = "Natural and Physical Sciences\n2006 - 2023"
    ) +
    theme(
      legend.position = "none"
    )
}

make_fig22 <- function(disciplines_combined) {
  ggplot(disciplines_combined) +
    aes(x = Age, y = Migrants, group = Year, color = Year) +
    geom_line() +
    facet_wrap(~discipline, scales = "free_y") +
    scale_x_continuous(breaks = seq(10, 100, by = 10)) +
    scale_color_gradientn(
      colours = rainbow(10)
    ) +
    labs(
      y = "Number of net migrants",
      title = "Estimated Net Migration by Discipline",
      subtitle = "Natural and Physical Sciences\n2006 - 2020"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    theme(
      legend.position = "top"
    )
}

make_fig23 <- function(future_disciplines_combined) {
  eg_forecast <- future_disciplines_combined |>
    as_tibble() |>
    filter(Year %in% c(2025, 2035)) |>
    group_by(discipline, Age, Year) |>
    summarise(
      mean = mean(Working),
      lo = quantile(Working, prob = 0.05),
      hi = quantile(Working, prob = 0.95),
      .groups = "drop"
    )

  # Overlay
  ggplot(eg_forecast) +
    aes(x = Age, color = factor(Year)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = factor(Year)), alpha = 0.1) +
    geom_line(aes(y = mean), linewidth = 0.75) +
    labs(
      y = "Number of working scientists",
      title = "Forecast of Working Population by Discipline",
      subtitle = "Natural and Physical Sciences",
      color = "Forecast Year",
      fill = "Forecast Year"
    ) +
    scale_color_manual(values = c("2025" = "#66666644", "2035" = "#c14b14")) +
    scale_fill_manual(values = c("2025" = "#66666644", "2035" = "#c14b1444")) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    theme(
      legend.position = "top"
    ) +
    facet_wrap(~discipline, scales = "free_y")
}

make_fig24 <- function(disciplines_combined, future_disciplines_combined) {
  sum_disciplines_combined <- disciplines_combined |>
    as_tibble() |>
    group_by(discipline, Year) |>
    summarise(Working = sum(Working), .groups = "drop")

  sum_future_disciplines_combined <- future_disciplines_combined |>
    as_tibble() |>
    group_by(discipline, Year, .rep) |>
    summarise(Working = sum(Working), .groups = "drop") |>
    group_by(discipline, Year) |>
    summarise(
      mean = mean(Working),
      lo = quantile(Working, prob = 0.05),
      hi = quantile(Working, prob = 0.95),
      .groups = "drop"
    )

  ggplot(sum_future_disciplines_combined) +
    aes(x = Year) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = sum_disciplines_combined, aes(y = Working)) +
    labs(
      y = "Total number of working scientists (thousands)",
      title = "Forecast of Total Working Population by Discipline",
      subtitle = "Natural and Physical Sciences\nForecasted Years: 2022 - 2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 10)) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    facet_wrap(~discipline, scales = "free_y")
}

make_fig_grad_forecasts <- function(grads, future_grads) {
  future_grads <- future_grads |>
    group_by(discipline) |>
    summarise(
      mean = mean(Graduates),
      lo = quantile(Graduates, prob = 0.05),
      hi = quantile(Graduates, prob = 0.95)
    )
  grads <- grads |>
    as_tibble() |>
    group_by(discipline, Year) |>
    summarise(Graduates = sum(Graduates))
  future_grads |>
    as_tibble() |>
    ggplot() +
    aes(x = Year, group = discipline) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c14b1444") +
    geom_line(aes(y = mean), color = "#c14b14") +
    geom_line(data = grads, aes(y = Graduates)) +
    labs(
      y = "Number of graduates",
      title = "Forecast of Graduates",
      subtitle = "Natural and Physical Sciences\nForecasted Years: 2024–2041"
    ) +
    scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
    facet_wrap(~discipline, scales = "free_y")
}
