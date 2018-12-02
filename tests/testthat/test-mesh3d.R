context("test-mesh3d")

test_that("mesh3d functions work", {

  # A cube is its own bounding box
  cube <- mesh3dobj$cube
  bb <- create_bbox_obj(cube)
  bb$normals <- NULL
  expect_equal(bb$vb, cube$vb)
  expect_equal(bb$ib, cube$ib)



  # print mesh3d doesn't barf
  cube$properties <- cube$vb
  res <- print(cube)


  info <- get_object_info(cube)
  expect_equal(info$vertices, 8)
  expect_equal(unname(info$elements), 6)
})
