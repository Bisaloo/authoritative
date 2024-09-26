test_that("remove_brackets() works in simple cases", {
  expect_identical(
    remove_brackets("Wolfgang Amadeus Mozart (1756-1791)"),
    "Wolfgang Amadeus Mozart "
  )

  expect_identical(
    remove_brackets("A sourced claim [1]", bracket = "["),
    "A sourced claim "
  )

  expect_identical(
    remove_brackets("A sourced claim [1] [12]", bracket = "["),
    "A sourced claim  "
  )
})

test_that("remove_brackets() works with nested brackets", {
  expect_identical(
    remove_brackets("A sourced claim (source: [1])", bracket = "("),
    "A sourced claim "
  )

  expect_identical(
    remove_brackets("A sourced claim (source: [1])", bracket = "["),
    "A sourced claim (source: )"
  )
})
