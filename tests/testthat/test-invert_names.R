test_that("invert_names() detects 'First Last' duplicates of 'Last First'", {
  expect_identical(
    invert_names(
      c("Wolfgang Mozart", "Mozart Wolfgang", "Bach Johannes", "Johannes Bach"),
      c("Wolfgang Mozart", "Johannes Bach")
    ),
    rep(c("Wolfgang Mozart", "Johannes Bach"), each = 2)
  )

  # Most common occurrence is returned
  nms <- c("Wolfgang Mozart", "Mozart Wolfgang", "Wolfgang Mozart")
  expect_identical(
    invert_names(
      nms,
      nms
    ),
    rep_len("Wolfgang Mozart", 3)
  )

  # Names without a match are untouched
  expect_identical(
    invert_names(
      c("Wolfgang Mozart", "Mozart Wolfgang", "Johannes Bach"),
      "Wolfgang Mozart"
    ),
    c(rep_len("Wolfgang Mozart", 2), "Johannes Bach")
  )
})
