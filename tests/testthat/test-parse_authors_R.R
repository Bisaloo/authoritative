test_that("parse_authors_r() snapshot", {
  # This seems to be R version dependent?
  skip_on_ci()

  cran_epidemiology_packages$`Authors@R` |>
    parse_authors_r() |>
    unique() |>
    expect_snapshot(variant = as.character(getRversion()))
})
