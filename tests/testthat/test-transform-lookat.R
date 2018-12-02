context("test-transform-lookat")

test_that("transform_lookat() works", {
  m <- look_at_matrix(eye = c(10, 0, 0), at = c(0, 0, 0))

  expected_m <- matrix(c(0, 0, -1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 10, 0, 0, 1), nrow=4)

  expect_equal(m, expected_m)

})
