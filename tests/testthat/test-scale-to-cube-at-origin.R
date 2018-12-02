context("test-scale-to-cube-at-origin")

test_that("multiplication works", {

  m   <- mesh3dobj$cube
  expect_false('transform_matrix' %in% names(m))

  res <- scale_to_cube_at_origin(m)
  expect_equal(res$transform_matrix, identity_matrix())


  cube_df <- as.data.frame(mesh3dobj$cube)
  cube_df <- cube_df[1,]
  cube_df$x <- Inf
  cube_df$y <- 0
  cube_df$z <- 0
  cube    <- as.mesh3d(cube_df)
  expect_error(scale_to_cube_at_origin(cube), "Non-finite vertex extents")

})
