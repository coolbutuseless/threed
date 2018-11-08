

library(dplyr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Borrowing basic objects from 'rgl'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cube          <- rgl::cube3d()
tetrahedron   <- rgl::tetrahedron3d()
octahedron    <- rgl::octahedron3d()
icosahedron   <- rgl::icosahedron3d()
dodecahedron  <- rgl::dodecahedron3d()
cuboctahedron <- rgl::cuboctahedron3d()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sphere - it's a bit dodgy, so tidy it up first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sphere <- rgl::readOBJ("obj/sphere.obj")
sphere$ib[-1, ] -> sphere$it
sphere$ib <- NULL

sphere <- sphere %>%
  threed::scale_to_cube_at_origin(size = 2) %>%
  threed::actualize_transformation()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bird - Eames house bird
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bird <- rgl::readOBJ("obj/eames_house_bird.obj") %>%
#   threed::scale_to_cube_at_origin(size = 2) %>%
#   threed::actualize_transformation()
#
# bird$texcoords <- NULL



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bunny
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bunny <- rgl::readOBJ("obj/bunny.obj") %>%
  threed::scale_to_cube_at_origin(size = 2) %>%
  threed::actualize_transformation()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cow
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cow <- rgl::readOBJ("obj/cow-nonormals.obj") %>%
  threed::scale_to_cube_at_origin(size = 2) %>%
  threed::actualize_transformation()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Teapot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
teapot <- rgl::readOBJ("obj/teapot.obj") %>%
  threed::scale_to_cube_at_origin(size = 2) %>%
  threed::actualize_transformation()

usethis::use_data(cube, compress = 'xz', overwrite = TRUE, internal = TRUE)


mesh3dobj <- list(
  cube          = cube,
  tetrahedron   = tetrahedron,
  octahedron    = octahedron,
  icosahedron   = icosahedron,
  dodecahedron  = dodecahedron,
  cuboctahedron = cuboctahedron,
  sphere        = sphere,
  bunny         = bunny,
  cow           = cow,
  teapot        = teapot
)


usethis::use_data(mesh3dobj, compress = 'xz', overwrite=TRUE)





if (FALSE) {
  library(threed)
  camera_to_world <- look_at_matrix(eye = c(1.5, 1.75, 4), at = c(0, 0, 0))

  # obj <- rgl::readOBJ("obj/sphere.obj")
  # obj$ib[-1, ] -> obj$it
  # obj$ib <- NULL

  obj <- bird %>%
    transform_by(invert_matrix(camera_to_world)) %>%
    perspective_projection()

  ggplot(obj, aes(x, y, z=z, group=zorder)) +
    geom_polygon(aes(fill=zorder, colour=zorder)) +
    theme_minimal() +
    theme(legend.position = 'none') +
    coord_equal() +
    scale_fill_viridis_d (option = 'A') +
    scale_color_viridis_d(option = 'A')

}