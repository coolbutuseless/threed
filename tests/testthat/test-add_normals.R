context("add_normals")

test_that("multiplication works", {

  cube_vertex_normals <- structure(
    c(-0.577350269189626, -0.577350269189626, -0.577350269189626,
      1, 0.577350269189626, -0.577350269189626, -0.577350269189626,
      1, -0.577350269189626, 0.577350269189626, -0.577350269189626,
      1, 0.577350269189626, 0.577350269189626, -0.577350269189626,
      1, -0.577350269189626, -0.577350269189626, 0.577350269189626,
      1, 0.577350269189626, -0.577350269189626, 0.577350269189626,
      1, -0.577350269189626, 0.577350269189626, 0.577350269189626,
      1, 0.577350269189626, 0.577350269189626, 0.577350269189626, 1
    ), .Dim = c(4L, 8L)
  )

  cube_face_normals <- structure(
    c(0, 0, -1, 1, 0, 1, 0, 1, 1, 0, 0, 1, -1, 0, 0, 1,
      0, -1, 0, 1, 0, 0, 1, 1), .Dim = c(4L, 6L)
  )

  this_cube              <- threed::mesh3dobj$cube
  this_cube$normals      <- NULL
  this_cube$face_normals <- NULL
  this_cube              <- add_normals(this_cube)

  expect_equal(this_cube$normals     , cube_vertex_normals)
  expect_equal(this_cube$face_normals, cube_face_normals)


})
