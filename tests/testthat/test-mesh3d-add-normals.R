context("test-mesh3d-add-normals")

test_that("add_normals() works correctly for line objects", {

  cube <- mesh3dobj$cube

  # turn into lines object
  cube$il <- cube$ib
  cube$ib <- NULL

  # No normals added to a lines object
  cube <- add_normals(cube)
  expect_null(cube$normals)

})

test_that("add_normals() works for non-homogenous coords", {

  cube <- mesh3dobj$cube

  # turn into lines object
  cube$vb <- cube$vb[1:3,]

  # No normals added to a lines object
  cube <- add_normals(cube)
  expect_type(cube$normals, 'double')

})
