# Level 4
read_course_leavers <- function(file) {
  out <- readxl::read_excel(file, sheet = "4-digit") |>
    transmute(
      category = `Broad Field of Education`,
      discipline = `Narrow Field of Education`,
      year = as.integer(Year),
      graduates = Sum
    ) |>
    filter(category == "Natural and Physical Sciences") |>
    select(-category)

  # Combine "Other Natural and Physical Sciences" and "Natural and Physical Sciences (n.f.d.)"
  out$discipline <- if_else(
    out$discipline == "Natural and Physical Sciences (n.f.d.)" |
      out$discipline == "Natural and Physical Sciences, nfd",
    "Other Natural and Physical Sciences",
    out$discipline
  )
  out |>
    group_by(year, discipline) |>
    summarise(graduates = sum(graduates), .groups = "drop")
}
