context("test-transform-by")

test_that("transform_by() throws error if type unknown", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))

  # Can't transform a character
  expect_error(transform_by('a', I))


})



test_that("transform_by() works on matrices", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))

  set.seed(1)
  points <- matrix(sample(1:16, 10 * 4, replace = TRUE), ncol = 10)
  points[4, ] <- 1

  expect_identical(transform_by(points, I), points)

  expect_identical(transform_by(t(points), I), t(points))


  m <- matrix(0, ncol = 3, nrow = 3)
  expect_error(transform_by(m, I), "Non-sane")

})




test_that("transform_by() works on mesh3d objects", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))
  cube <- mesh3dobj$cube

  cube <- transform_by(cube, T)

  expect_identical(cube$transform_matrix, T)
})




test_that("transform_by() works on vector objects", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))

  vec <- c(1, 2, 3, 1)
  vec <- transform_by(vec, T)
  expect_equal(vec, c(2, 4, 6, 1))

  vec <- c(1, 2, 3)
  vec <- transform_by(vec, T)
  expect_equal(vec, c(2, 4, 6))

  vec <- c(1, 2)
  expect_error(vec <- transform_by(vec, T), "Bad length")
})



test_that("transform_by() works on mesh3dlist objects", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))

  ll <- mesh3dlist(mesh3dobj$cube, mesh3dobj$cube)

  ll <- transform_by(ll, T)

  expect_true('mesh3dlist' %in% class(ll))
  expect_length(ll, 2)
  expect_equal(ll[[1]]$transform_matrix, T)
  expect_equal(ll[[2]]$transform_matrix, T)

})




test_that("transform_by_inverse() works", {
  I <- identity_matrix()
  T <- translation_matrix(c(1, 2, 3))

  vec <- c(1, 2, 3, 1)

  vec <- transform_by_inverse(vec, T)

  expect_equal(vec, c(0, 0, 0, 1))
})
