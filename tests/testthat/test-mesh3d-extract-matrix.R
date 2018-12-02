context("test-mesh3d-extract-matrix")

test_that("mesh3d extrac matrix works", {

  cube <- mesh3dobj$cube

  expect_type(get_matrix_for_element_type(cube, 4), 'list')
  expect_null(get_matrix_for_element_type(cube, 3))


  # turn it into lines
  cube$il <- cube$ib
  cube$ib <- NULL
  expect_type(get_matrix_for_element_type(cube, 2), 'list')

  # turn it into points
  cube$ip <- cube$il
  cube$il <- NULL
  expect_type(get_matrix_for_element_type(cube, 1), 'list')




  cube <- mesh3dobj$cube
  cube <- threed::add_normals(cube)
  expect_type(get_matrix_for_element_type(cube, 4), 'list')


})
