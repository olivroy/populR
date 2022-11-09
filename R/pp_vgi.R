#' Download and Count OSM Features Over Target
#'
#' @param x an object of class \code{sf} that is used to interpolate data
#'     to. Usually, x may include polygon features representing building units
#' @param key osm feature key see \link[osmdata]{available_features}
#' @param fun select between nearest and st_intersects
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
#' @importFrom units drop_units
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

pp_vgi <- function(x, key, fun = st_intersects, value = NULL) {
  if (missing(x)) {
    usethis::ui_stop('x is required')
  }
  if (missing(key)) {
    usethis::ui_stop('key is required')
  }
  if (missing(fun)) {
    usethis::ui_stop('fun is required')
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

  fun <- rlang::quo_name(rlang::enquo(fun))
  f <- c('st_intersects', 'nearest')
  if (!fun %in% f) {
    usethis::ui_stop('{fun} must be either st_intersects or nearest')
  }

  bb <- sf::st_bbox(x)
  if (is.null(value)) {
    data <- opq(bbox = bb) %>% add_osm_feature(key = key) %>% osmdata_sf()
    data <- data$osm_points
    if (fun == 'st_intersects') {
      x[, key] <- lengths(st_intersects(x, data))
    } else if (fun == 'nearest') {
     x <- pp_nearest(x, data, key)
    }
  } else {
    tags <- osmdata::available_tags(key)
    value <- paste(value)
    for (i in 1:length(value)){
      if (!value[i] %in% tags) {
        usethis::ui_stop('{value[i]} is not a valid OSM tag of {key}')
      }
      data <- opq(bbox = bb) %>% add_osm_feature(key = key, value = value[i]) %>% osmdata_sf()
      data <- data$osm_points
      if (fun == 'st_intersects') {
        x[, value[i]] <- lengths(st_intersects(x, data))
      } else if (fun == 'nearest') {
        x <- pp_nearest(x, data, value[i])
      }
    }
  }
  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  x <- sf::st_zm(x)
  if (x_crs != t_crs) {
    x <- sf::st_transform(x, x_crs)
  }
  return(x)
}
