context("test-mesh3dlist")

test_that("mesh3dlist() works", {
  a <- mesh3dlist(1)
  expect_type(a, 'list')
  expect_true('mesh3dlist' %in% class(a))
})
