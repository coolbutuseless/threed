context("test-element-types")

test_that("element_type functions work", {
  # cuboctohedron has both tris and quads
  res <- get_element_types(mesh3dobj$cuboctahedron)
  expect_equal(res, c(3, 4))


  df <- as.data.frame(mesh3dobj$cuboctahedron)
  res <- get_element_types(df)
  expect_equal(res, c(3, 4))

})
