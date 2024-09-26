test_that("parse_authors() snapshot", {
  cran_epidemiology_packages$Author |>
    parse_authors() |>
    unique() |>
    expect_snapshot()
})
