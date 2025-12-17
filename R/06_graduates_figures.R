make_fig_grad_forecasts <- function(
  grads,
  future_grads,
  no_other = TRUE,
  PI = TRUE
) {
  future_grads <- future_grads |>
    as_tibble() |>
    filter(year <= 2035)
  grads <- grads |>
    as_tibble()
  if (no_other) {
    grads <- grads |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
    future_grads <- future_grads |>
      dplyr::filter(discipline != "Other Natural and Physical Sciences")
  }
  p <- grads |>
    ggplot() +
    aes(x = year, group = discipline) +
    geom_line(aes(y = graduates)) +
    labs(
      x = "Year",
      y = "Number of graduates",
      title = "Forecasts of total graduates by discipline",
    ) +
    scale_x_continuous(breaks = seq(2010, 2035, by = 5)) +
    facet_wrap(~discipline, scales = "free_y")
  if (PI) {
    future_grads <- future_grads |>
      group_by(discipline, year) |>
      summarise(
        mean = mean(graduates),
        lo = quantile(graduates, prob = 0.05),
        hi = quantile(graduates, prob = 0.95),
        .groups = "drop"
      )
    p <- p +
      geom_ribbon(
        data = future_grads,
        aes(ymin = lo, ymax = hi),
        fill = "#c14b1444"
      ) +
      geom_line(data = future_grads, aes(y = mean), color = "#c14b14")
  } else {
    p <- p +
      geom_line(
        data = future_grads |> filter(.rep %in% as.character(1:10)),
        aes(y = graduates, group = .rep, colour = .rep)
      ) +
      guides(colour = "none")
  }
  p
}

make_fig_completions <- function(
  completions,
  ave_completions = NULL,
  sd_completions = NULL,
  by_year = TRUE,
  average = FALSE,
  pi = FALSE
) {
  if (average & (is.null(ave_completions) | is.null(sd_completions))) {
    stop("Please provide ave_completions and sd_completions if average = TRUE")
  }
  if (average & !by_year) {
    title <- "Average graduate completions"
  } else if (by_year) {
    title <- "Graduate completions"
  } else {
    stop("Please select either by_year or average")
  }
  p <- ggplot(completions) +
    aes(x = age, y = pc) +
    labs(
      x = "Age",
      y = "Percentage of graduates",
      title = title
    )
  if (by_year & !pi) {
    p <- p +
      geom_step(
        aes(colour = year, group = year),
        alpha = 0.2 + 0.8 * (!average)
      ) +
      scale_color_gradientn(colours = rainbow(10), name = "Year")
  }
  if (average) {
    if (pi) {
      df <- ave_completions |>
        rename(mean = pc) |>
        left_join(sd_completions, by = "age") |>
        rename(sd = pc) |>
        mutate(
          lower = pmax(0, mean - 2 * sd),
          upper = mean + 2 * sd
        )
      p <- p +
        geom_ribbon(
          data = df,
          aes(x = age, y = mean, ymin = lower, ymax = upper),
          fill = "gray",
          alpha = 0.9
        )
    }
    p <- p + geom_line(data = ave_completions)
  }
  p +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
}
