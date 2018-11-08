if (vertices_per_element > 2) {
  obj <- add_normals(obj)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # vertex normals. ensure they are unit vectors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ('normals' %in% names(obj) && !is.null(obj$normals)) {
    vertex_normals <- t(obj$normals)[vertex_indices, 1:3]
    vertex_normals <- t(apply(vertex_normals, 1, vec3_normalize))
    colnames(vertex_normals) <- c('vnx', 'vny', 'vnz')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # face normals
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ('face_normals' %in% names(obj) && !is.null(obj$face_normals)) {
    face_normals <- t(obj$face_normals)[,1:3]
    colnames(face_normals) <- c('fnx', 'fny', 'fnz')
    face_normals <- face_normals[rep(seq(n_elements), each=vertices_per_element),]
  }
}