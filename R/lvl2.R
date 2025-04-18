read_census2 <- function(file) {
  # Numbers of people at census
  y2006 <- read_census_year(file, "2006")
  y2011 <- read_census_year(file, sheet = "2011")
  y2016 <- read_census_year(file, sheet = "2016 - Labour force flag")
  y2021 <- read_census_year(file, sheet = "2021 - Labour force flag")

  # Combine years
  census <- bind_rows(y2006, y2011, y2016, y2021) |>
    ave_smooth_pr() |>
    mutate(
      working = if_else(
        is.na(participation),
        persons * .smooth,
        persons * participation
      )
    )
  return(census)
}
