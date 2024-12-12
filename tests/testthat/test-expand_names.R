test_that("expand_names() transforms matching names", {
  
  # Expanded without middle name
  expect_identical(
    expand_names(
      c("W Mozart", "Wolfgang Mozart"),
      "Wolfgang Mozart"
    ),
    rep_len("Wolfgang Mozart", 2)
  )
      
  # Expanded ith middle name
  expect_identical(
    expand_names(
      c("W A Mozart", "Wolfgang Mozart", "Wolfgang A Mozart"),
      "Wolfgang Amadeus Mozart"
    ),
    rep_len("Wolfgang Amadeus Mozart", 3)
  )

  expect_identical(
    expand_names(
      c("Wolfgang Mozart", "Johannes Bach"),
      c("Johannes Sebastian Bach", "Wolfgang Amadeus Mozart")
    ),
    c("Wolfgang Amadeus Mozart", "Johannes Sebastian Bach")
  )
})

test_that("expand_names() favours longer forms", {
  expect_identical(
    expand_names(
      c("W A Mozart", "Wolfgang Mozart", "Wolfgang A Mozart"),
      c("W A Mozart", "Wolfgang Amadeus Mozart", "Wolfgang A Mozart")
    ),
    rep_len("Wolfgang Amadeus Mozart", 3)
  )
})

test_that("expand_names() leaves non-matching names untouched", {
  expect_identical(
    expand_names(
      c("Johannes Bach", "Wolfgang Mozart"),
      "Wolfgang Amadeus Mozart"
    ),
    c("Johannes Bach", "Wolfgang Amadeus Mozart")
  )
})
