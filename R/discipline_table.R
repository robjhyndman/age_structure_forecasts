make_discipline_table <- function() {
  tibble::tibble(
    Narrow_Discipline = c(
      "Physics and Astronomy",
      "Mathematical Sciences",
      "Chemical Sciences",
      "Earth Sciences",
      "Biological Sciences",
      "Other Natural and Physical Sciences"
    ),
    Detailed_Field = c(
      "Physics, Astronomy.",
      "Mathematics, Statistics, Mathematical Sciences, n.e.c.",
      "Organic Chemistry, Inorganic Chemistry, Chemical Sciences, n.e.c.",
      "Atmospheric Sciences, Geology, Geophysics, Geochemistry, Soil Science, Hydrology, Oceanography, Earth Sciences, n.e.c.",
      "Biochemistry and Cell Biology, Botany, Ecology and Evolution, Marine Science, Genetics, Microbiology, Human Biology, Zoology, Biological Sciences, n.e.c.",
      "Medical Science, Forensic Science, Food Science and Biotechnology, Pharmacology, Laboratory Technology, Natural and Physical Sciences, n.e.c."
    )
  ) |>
    kableExtra::kbl(
      format = "latex",
      col.names = c("Narrow Fields", "Detailed Fields")
    ) |>
    kableExtra::add_header_above(
      c("Broad Field: Natural and Physical Sciences" = 2),
      bold = TRUE
    ) |>
    kableExtra::kable_styling(
      latex_options = c("striped"),
      full_width = FALSE
    ) |>
    kableExtra::column_spec(2, width = "20em")
}
