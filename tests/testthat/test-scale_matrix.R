context("scaling_matrix")

test_that("scaling_matrix works", {
  mat <- scaling_matrix(vec3(1, 2, 4))


  ref <- matrix(
    c(1, 0, 0, 0,
      0, 2, 0, 0,
      0, 0, 4, 0,
      0, 0, 0, 1), byrow = TRUE, nrow=4)


  expect_equal(mat, ref)
})
