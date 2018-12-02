context("test-mesh3d-conversion")

test_that("mesh3d conversion works", {
  cube <- mesh3dobj$cube

  cube_df <- as.data.frame(cube)

  cube_df$fnz <- NULL

  cube_df$test <- 'a'
  cube_df$element_id <- 1L

  cube_final <- as.mesh3d(cube_df)
  expect_true(all(cube_final$properties$test == 'a'))

  cube_df$x <- NULL
  expect_error(as.mesh3d(cube_df), "missing columns")




  cube <- mesh3dobj$cuboctahedron
  cube_df <- as.data.frame(cube)
  cube_df$vertex <- NULL
  res <- as.mesh3d(cube_df)

})
