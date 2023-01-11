#' Download and Count OSM Features Over Target
#'
#' @param x an object of class \code{sf} that is used to interpolate data
#'     to. Usually, x may include polygon features representing building units
#' @param key osm feature key see \link[osmdata]{available_features}
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
#' @importFrom osmdata add_osm_features
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
#'     pp_vgi(trg, key = amenity)
#' }
#'

pp_vgi <- function(x, key) {
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

  af <- osmdata::available_features()
  for (i in 1:length(key)) {
    if (!key[i] %in% af) {
      usethis::ui_stop('{key[i]} is not a valid OSM feature')
    }
  }

  key <- sprintf("\"%s\"", key)

  bb <- sf::st_bbox(x)
  dt <- opq(bbox = bb) %>%
    add_osm_features(features = key) %>%
    osmdata_sf()

  pt <- dt$osm_points
  pt <- pt[, c('osm_id', 'geometry')]

  pl <- dt$osm_polygons
  pl <- suppressWarnings(st_centroid(pl))
  pl <- pl[, c('osm_id', 'geometry')]

  fdt <- rbind(pt, pl)

  fdt <- sf::st_transform(fdt, x_crs)
  x <- sf::st_transform(x, x_crs)

  x$pp_vgi <- lengths(st_intersects(x, fdt))


  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  x <- sf::st_zm(x)

  return(x)
}
