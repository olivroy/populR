#' Download and Count OSM Features Over Target
#'
#' @param x an object of class \code{sf}
#' @param key osm feature key
#' @param value osm feature value (tag)
#'
#' @importFrom usethis ui_stop
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_bbox
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom osmdata available_features
#' @importFrom osmdata
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#'
#' @return an object of class \code{sf} including OSM features
#' @noRd
#'

pp_osm <- function(x, key, value = NULL) {
  if (missing(x)) {
    usethis::ui_stop('x is required')
  }
  if (missing(key)) {
    usethis::ui_stop('key is required')
  }

  xc <- "sf" %in% class(x)
  if (xc == FALSE) {
    usethis::ui_stop('{x} must be an object of class sf')
  }

  x_crs <- sf::st_crs(x, parameters = TRUE)$srid
  t_crs <- 'EPSG:4326'
  if (x_crs != t_crs) {
    x <- sf::st_transform(x, t_crs)
  }

  key <- rlang::quo_name(rlang::enquo(key))
  af <- osmdata::available_features()
  if (!key %in% af) {
    usethis::ui_stop('{key} is not a valid OSM feature')
  }

  bb <- sf::st_bbox(x)
  if (is.null(value)) {
    data <- opq(bbox = bb) %>% add_osm_feature(key = key) %>% osmdata_sf()
    data <- data$osm_points
    x[, key] <- lengths(st_intersects(x, data))
  } else {
    value <- rlang::quo_name(rlang::enquo(value))
    data <- opq(bbox = bb) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()
    x[, value] <- lengths(st_intersects(x, data))
  }

  x <- sf::st_zm(x)
  return(x)
}
