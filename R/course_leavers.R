# Level 4
read_course_leavers <- function(file) {
  readxl::read_excel(file, sheet = "4-digit") |>
  transmute(
    category = `Broad Field of Education`,
    discipline = `Narrow Field of Education`,
    Year = as.integer(Year),
    Graduates = Sum
  ) |>
  filter(category == "Natural and Physical Sciences")
}

