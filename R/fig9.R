make_fig9 <- function(completions) {
  completions |>
    ggplot() +
    aes(x = age, y = pc, colour = year, group = year) +
    geom_line() +
    scale_color_gradientn(colours = rainbow(10)[1:8]) +
    labs(
      y = "Percentage of graduates",
      title = "Graduate Completions by Year and Age",
      subtitle = "2006 - 2023"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1)
    )
}

make_fig10 <- function(ave_completions) {
  ave_completions |>
    ggplot() +
    aes(x = age, pc) +
    geom_line() +
    labs(y = "Percentage of graduates") +
    labs(
      title = "Graduate Completions by Age",
      subtitle = "Averaged over 2006 – 2023"
    ) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
}

make_table2 <- function(ave_completions) {
  ave_completions |>
    filter(age >= 20 & age <= 25) |>
    mutate(pc = paste0(round(pc, 2), "%")) |>
    select(age, pc) |>
    kable(col.names = c("Age", "Percentage of Graduates"), align = "c") |>
    add_header_above(
      c("Percentage of Graduates Ages 20-25" = 2),
      bold = TRUE
    ) |>
    kable_styling(latex_options = c("striped"))
}
