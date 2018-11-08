context("conversion")

test_that("conversion back-and-forth works", {

  obj_name <- 'cube'

  for (obj_name in names(threed::mesh3dobj)) {
    # print(obj_name)

    obj1 <- threed::mesh3dobj[[obj_name]]
    df1  <- as.data.frame(obj1)
    obj2 <- as.mesh3d(df1)
    df2  <- as.data.frame(obj2)

    expect_identical(df1, df2)

    obj2$normals <- NULL
    obj2$face_normals <- NULL

    # common_items <- intersect(names(obj1), names(obj2))
    common_items <- intersect(names(obj1), c('vb', 'ib', 'it'))
    expect_true('ib' %in% common_items || 'it' %in% common_items)
    expect_true('vb'           %in% common_items)

    for (item in common_items) {
      # print(item)
      expect_equal(obj1[[item]], obj2[[item]])
    }
  }

})
