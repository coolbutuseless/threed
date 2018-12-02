context("test-project-perspective")

test_that("project_perspecitve() works", {
  m   <- mesh3dobj$cube
  expect_false('transform_matrix' %in% names(m))

  res <- perspective_projection(m)

  expect_true('mesh3d' %in% class(res))
  expect_true('transform_matrix' %in% names(res))

  expect_transform_matrix <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1.22222222222222,
                                      -1, 0, 0, 2.22222222222222, 0), nrow = 4)

  expect_equal(res$transform_matrix, expect_transform_matrix)
})
