test_that("parse_authors_r() snapshot", {
  
  cran_epidemiology_packages$`Authors@R` |>
    parse_authors_r() |> 
    unique() |> 
    expect_snapshot()
  
})
