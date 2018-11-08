context("translation_matrix")

test_that("translation_matrix works", {
  mat <- translation_matrix(vec3(1, 2, 3))

  ref <- matrix(
    c(1, 0, 0, 1,
      0, 1, 0, 2,
      0, 0, 1, 3,
      0, 0, 0, 1), byrow = TRUE, nrow=4)

  expect_identical(mat, ref)
})
