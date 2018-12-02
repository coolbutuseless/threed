context("test-transform-rotate")

test_that("transform_rotate() works", {

  expect_equal(rotation_matrix(0, 1), identity_matrix())


  res <- rotate_by(mesh3dobj$cube, 2*pi, c(0, 0, 1))

  expect_equal(res$transform_matrix, identity_matrix())

})
