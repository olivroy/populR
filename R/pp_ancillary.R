#' Ancillary Information from OSM Features
#'
#' @param x an object of class \code{sf} that is used to associate OSM features
#'     to. Usually, x may include polygon features representing building units
#' @param volume x volume information (height or number of floors) useful for
#'     float ancillary information
#' @param key OSM feature keys or values available in x
#'
#' @importFrom usethis ui_stop
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#'
#' @return an object of class \code{sf} including ancillary information either for
#'     night or day estimates
#' @export
#'
#' @examples
#' \dontrun{
#'     data('trg')
#'
#'     # Download OSM amenities
#'     dt <- pp_vgi(trg, key = amenity)
#'
#'     # create binary ancillary information
#'     dt <- pp_ancillary(dt, 'amenity')
#'
#'     # create ancillary information both binary and float
#'     dt <- pp_ancillary(dt, floors, 'amenity')
#' }
#'
pp_ancilalry <- function(x, volume = NULL, key) {
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

  volume <- rlang::quo_name(rlang::enquo(volume))
  key <- paste(key)

  for (i in 1:length(key)) {
    k <- key[i]
    if (!k %in% colnames(x)) {
      usethis::ui_stop('{k} cannot be found')
    }
  }

  x$vgi <- 0
  for (i in 1:length(key)) {
    x$vgi <- x$vgi + x[, key[i], drop = TRUE]
  }

  x$bin_night <- 1
  x$bin_night <- x$bin_night - x$vgi
  x$bin_night[x$bin_night < 1] <- 0

  x$bin_day <- x$vgi
  x$bin_day[x$bin_day > 1] <- 1

  if (volume != 'NULL') {
    if (volume != 'NULL') {
      if (!volume %in% colnames(x)) {
        usethis::ui_stop('{volume} cannot be found')
      }
    }
    x$flt_day <- x$vgi/x[, volume, drop = TRUE]
    x$flt_night <- 1 - x$flt_day
    x$flt_night[x$flt_night <= 0] <- 0
  }
  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  return(x)
}
