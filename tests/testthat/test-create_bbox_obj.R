context("create_bbox_obj")

test_that("bounding box of default cube should be identical to default cube", {
  cube <- threed:::cube

  bbox <- create_bbox_obj(cube)

  bbox$transform_matrix <- NULL
  cube$normals <- NULL

  expect_identical(bbox, cube)
})



test_that("bounding box of bunny is same size as bunny", {
  bunny <- mesh3dobj$bunny
  bbox  <- create_bbox_obj(bunny)

  bunny_ranges <- apply(bunny$vb, 1, range)
  bbox_ranges  <- apply(bbox$vb, 1, range)

  expect_equal(bunny_ranges, bbox_ranges)

})