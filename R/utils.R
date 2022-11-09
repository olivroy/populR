#' Internal Function - Areal Weighting Interpolation
#'
#' @param target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source pop
#' @param point whether to use point geometries
#'
#' @importFrom sf st_area
#' @importFrom sf st_centroid
#' @importFrom sf st_intersection
#'
#' @return an object of class \code{sf} including population estimates
#'     using awi
#' @noRd
#'

pp_awi <- function(target, source, sid, spop, point = FALSE) {

  target$pp_a <- as.vector(sf::st_area(target))

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # create wights field
  int$pp_w <- 0

  # find unique ids
  code <- unique(int[, sid, drop = TRUE])

  # calc weights
  for (i in 1:length(code)) {
    s <- as.numeric(sum(int$pp_a[int[, sid, drop = TRUE] == code[i]]))

    if (s > 0) {
      int$pp_w[int[, sid, drop = TRUE] == code[i]] <- int$pp_a[int[, sid, drop = TRUE] == code[i]]/s
    }
  }

  # calc target pop
  int$pp_est <- int[, spop, drop = TRUE] * int$pp_w

  return(int)
}


#' Internal Function - Volume Weighting Interpolation
#'
#' @param target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source population
#' @param volume target volume (height or number of floors)
#' @param point whether to use point geometries
#'
#' @importFrom sf st_area
#' @importFrom sf st_centroid
#' @importFrom sf st_intersection
#'
#' @return an object of class \code{sf} including population estimates
#'     using vwi
#' @noRd
#'
pp_vwi <- function(target, source, sid, spop, volume, point = FALSE) {

  target$pp_a <- as.vector(sf::st_area(target)) * target[, volume, drop = T]

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # create a new file to calc density
  int$pp_w <- 0

  # find unique ids
  code <- unique(int[, sid, drop = TRUE])

  # calc density for each source unit
  for (i in 1:length(code)) {
    s <- as.numeric(sum(int$pp_a[int[, sid, drop = TRUE] == code[i]]))

    if (s > 0) {
      int$pp_w[int[, sid, drop = TRUE] == code[i]] <- int$pp_a[int[, sid, drop = TRUE] == code[i]]/s
    }
  }

  # calc target pop
  int$pp_est <- int[, spop, drop = TRUE] * int$pp_w

  return(int)

}



#' Internal Function - Binary Dasymetric Mapping
#'
#' @param target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source population
#' @param ancillary target ancillary information
#' @param point whether to use point geometries
#'
#' @importFrom sf st_area
#' @importFrom sf st_centroid
#' @importFrom sf st_intersection
#'
#' @return an object of class \code{sf} including population estimates
#'     using bdi
#' @noRd
#'
pp_bdi <- function(target, source, sid, spop, ancillary, point = FALSE) {

  target$pp_a <- as.vector(sf::st_area(target)) * target[, ancillary, drop = T]

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # create a new file to calc density
  int$pp_w <- 0

  # find unique ids
  code <- unique(int[, sid, drop = TRUE])

  # calc density for each source unit
  for (i in 1:length(code)) {
    s <- as.numeric(sum(int$pp_a[int[, sid, drop = TRUE] == code[i]]))

    if (s > 0) {
      int$pp_w[int[, sid, drop = TRUE] == code[i]] <- int$pp_a[int[, sid, drop = TRUE] == code[i]]/s
    }
  }

  # calc target pop
  int$pp_est <- int[, spop, drop = TRUE] * int$pp_w

  return(int)

}

#' Internal Function - Float Dasymetric Mapping
#'
#' @param target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source population
#' @param volume target volume (height or number of floors)
#' @param ancillary target ancillary information
#' @param point whether to use point geometries
#'
#' @importFrom sf st_area
#' @importFrom sf st_centroid
#' @importFrom sf st_intersection
#'
#' @return an object of class \code{sf} including population estimates
#'     using fdi
#' @noRd
#'
pp_fdi <- function(target, source, sid, spop, volume, ancillary, point = FALSE) {

  target$pp_a <- as.vector(sf::st_area(target)) * target[, ancillary, drop = T] * target[, volume, drop = T]

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # create a new file to calc density
  int$pp_w <- 0

  # find unique ids
  code <- unique(int[, sid, drop = TRUE])

  # calc density for each source unit
  for (i in 1:length(code)) {
    s <- as.numeric(sum(int$pp_a[int[, sid, drop = TRUE] == code[i]]))

    if (s > 0) {
      int$pp_w[int[, sid, drop = TRUE] == code[i]] <- int$pp_a[int[, sid, drop = TRUE] == code[i]]/s
    }
  }

  # calc target pop
  int$pp_est <- int[, spop, drop = TRUE] * int$pp_w

  return(int)

}


#' Internal Function - Nearest OSM Features
#'
#' @param target an object of class \code{sf}
#' @param vgi OSM features in class \code{sf}
#' @param name OSM feature name
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_geometry_type
#' @importFrom sf st_centroid
#' @importFrom sf st_distance
#' @importFrom units drop_units
#'
#' @return nothing
#' @noRd
#'
pp_nearest <- function(target, vgi, name) {
  target$myid <- 1:nrow(target)

  name <- rlang::quo_name(rlang::enquo(name))

  vgi_geom <- as.character(sf::st_geometry_type(vgi, by_geometry = FALSE))
  target_geom <- as.character(sf::st_geometry_type(target, by_geometry = FALSE))

  pt <- 'POINT'
  if (!pt %in% vgi_geom) { vgi <- sf::st_centroid(vgi) }
  if (!pt %in% target_geom) { mirror <- sf::st_centroid(target) }

  dm <- as.matrix(sf::st_distance(vgi, mirror))
  dm <- units::drop_units(dm)

  bd <- 0
  for (i in 1:nrow(dm)) {
    bd[i] <- as.numeric(unique(which(min(dm[i,]) == dm[i,])))
  }

  df <- as.data.frame(table(bd))
  names(df) <- c('bld_id', 'nr')

  target[, 'vgi'] <- 0
  for (i in 1:nrow(df)) {
    target[df$bld_id[i] == mirror$myid, name] <- df$nr[i]
  }

  target$myid <- NULL
  return(target)
}
