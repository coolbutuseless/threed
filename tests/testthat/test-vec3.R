context("test-vec3")

test_that("vec3 functions work", {
  res <- vec3_crossproduct(1:3, 1:3)
  expect_equal(res, c(0, 0, 0))


  res <- vec3_crossproduct(c(0, 0, 1), c(0, 1, 0))
  expect_equal(res, c(-1, 0, 0))


  expect_equal(as.vector(vec3_dotproduct(1:3, 1:3)), 14)

  expect_equal(vec3_normalize(c(2, 2, 10)), c(0.192450089729875, 0.192450089729875, 0.962250448649376))

})



