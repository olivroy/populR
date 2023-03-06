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

  # internal tid
  target$pp_tid <- 1:nrow(target)

  # calc area
  target$pp_a <- as.vector(sf::st_area(target))

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)
  int <- int[table(int$pp_tid) == 1, ]

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

  # remove pp_tdi
  int$pp_tid <- NULL

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

  # internal tid
  target$pp_tid <- 1:nrow(target)

  # calc volume
  target$pp_a <- as.vector(sf::st_area(target)) * target[, volume, drop = T]

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # remove duplicate intersections
  int <- int[table(int$pp_tid) == 1, ]

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

  # remove pp_tid
  int$pp_tid <- NULL

  return(int)

}



#' Internal Function - Binary Dasymetric Mapping
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
#'     using bdi
#' @noRd
#'
pp_bdi <- function(target, source, sid, spop, volume = NULL, ancillary, point = FALSE) {

  # internal tid
  target$pp_tid <- 1:nrow(target)

  if (is.null(volume)) {
    target$pp_a <- as.vector(sf::st_area(target)) * target[, ancillary, drop = T]
  } else {
    target$pp_a <- as.vector(sf::st_area(target)) * target[, ancillary, drop = T] * target[, volume, drop = T]
  }


  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # remove duplicate intersections
  int <- int[table(int$pp_tid) == 1, ]

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

  # revome pp_tid
  int$pp_tid <- NULL

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

  # internal tid
  target$pp_tid <- 1:nrow(target)

  # calc volume
  target$pp_a <- as.vector(sf::st_area(target)) * target[, ancillary, drop = T] * target[, volume, drop = T]

  if (point) {
    target <- sf::st_centroid(target)
  }

  # perform intersection
  int <- sf::st_intersection(target, source)

  # remove duplicate intersections
  int <- int[table(int$pp_tid) == 1, ]

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

  # remove pp_tid
  int$pp_tid <- NULL

  return(int)

}
