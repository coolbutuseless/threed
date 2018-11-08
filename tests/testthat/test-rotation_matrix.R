context("rotation_matrix")

test_that("rotation_matrix works", {
  mat <- rotation_matrix(pi/2, vec3(0, 1, 0))


  ref <- matrix(
    c(0, 0, 1, 0,
      0, 1, 0, 0,
     -1, 0, 0, 0,
      0, 0, 0, 1), byrow = TRUE, nrow=4)


  expect_equal(mat, ref)
})
