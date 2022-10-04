#' Download and Count OSM Features Over Target
#'
#' @param x an object of class \code{sf} that is used to interpolate data
#'     to. Usually, x may include polygon features representing building units
#' @param key osm feature key see \link[osmdata]{available_features}
#' @param value osm feature value (tag) \link[osmdata]{available_tags}
#'
#' @importFrom usethis ui_stop
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_bbox
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom sf st_intersects
#' @importFrom osmdata available_features
#' @importFrom osmdata available_tags
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#'
#' @return an object of class \code{sf} including OSM features
#' @export
#'
#' @examples
#' \dontrun{
#'     data('trg')
#'
#'     # example using just key
#'     pp_vgi(trg, key = amenity)
#'
#'     # example using both key and value arguments
#'     pp_vgi(trg, key = amenity, value = pharmacy)
#' }
#'

pp_vgi <- function(x, key, value = NULL) {
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

  value <- rlang::quo_name(rlang::enquo(value))

  bb <- sf::st_bbox(x)
  if (value == 'NULL') {
    data <- opq(bbox = bb) %>% add_osm_feature(key = key) %>% osmdata_sf()
    data <- data$osm_points
    x[, key] <- lengths(st_intersects(x, data))
  } else {
    tags <- osmdata::available_tags(value)
    if (!value %in% tags) {
      usethis::ui_stop('{value} is not a valid OSM tag of {key}')
    }
    data <- opq(bbox = bb) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()
    data <- data$osm_points
    x[, value] <- lengths(st_intersects(x, data))
  }
  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  x <- sf::st_zm(x)
  return(x)
}
