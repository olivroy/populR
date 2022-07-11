#' Areal Interpolation of Population Data
#'
#' @param target an object of class \code{sf} representing individual
#'     building polygons
#' @param source an object of class \code{sf} representing coarse polygon
#'     features (such as census tracts or city blocks) including
#'     population counts
#' @param sid source id
#' @param spop source population
#' @param volume target volume (height or number of floors)
#' @param point whether to use point geometries (TRUE/FALSE)
#' @param method available methods: awi, vwi
#'
#' @return An object of class sf including estimated population
#'     counts for target zone features using either awi or vwi methods. The estimated population
#'     counts are stored in a new column called pp_est.
#' @export
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_crs
#' @importFrom usethis ui_stop
#'
#' @examples
#' # read lib data
#' data('src')
#' data('trg')
#'
#' # areal weighted interpolation - awi
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi)
#'
#' # areal weighted interpolation - awi using point geometries
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi, point = TRUE)
#'
#' # volume weighted interpolation - vwi
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # volume weighted interpolation - vwi using point geometries
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors, point = TRUE)
#'
pp_estimate <- function(target, source, sid, spop, volume = NULL, point = FALSE, method) {
  # check arguments
  if (missing(target)) {
    usethis::ui_stop('target is required')
  }

  if (missing(source)) {
    usethis::ui_stop('source is required')
  }

  if (missing(sid)) {
    usethis::ui_stop('sid is required')
  }

  if (missing(spop)) {
    usethis::ui_stop('spop is required')
  }

  if (missing(method)) {
    usethis::ui_stop('method is required')
  }

  # enquote args where necessary
  sid <- rlang::quo_name(rlang::enquo(sid))
  spop <- rlang::quo_name(rlang::enquo(spop))
  volume <- rlang::quo_name(rlang::enquo(volume))
  method <- rlang::quo_name(rlang::enquo(method))

  # check whether source and .target are of sf class
  sc <- "sf" %in% class(source)
  tc <- "sf" %in% class(target)

  if (sc == FALSE) {
    usethis::ui_stop("{source} must be an object of class sf")
  }

  if (tc == FALSE) {
    usethis::ui_stop('{target} must be an object of class sf')
  }

  # check whether source and target share the same crs
  if (sf::st_crs(target) != sf::st_crs(source)) {
    usethis::ui_stop('CRS mismatch')
  }

  # check whether params exist in the given source object
  if (!sid %in% colnames(source)) {
    usethis::ui_stop('{sid} cannot be found in the given source object')
  }

  if (!spop %in% colnames(source)) {
    usethis::ui_stop('{spop} cannot be found in the given source object')
  }

  # check whether method is valid
  m <- c('awi', 'vwi')

  if (!method %in% m) {
    usethis::ui_stop('{method} is not a valid method. Please choose between awi and vwi')
  }

  # check whether spop is numeric
  if (!is.numeric(source[, spop, drop = TRUE])) {
    usethis::ui_stop('{spop} must be numeric')
  }

  # check whether point is logical
  if (!is.logical(point)) {
    usethis::ui_stop('point must be either TRUE/T or FALSE/F')
  }

  # check whether sid and pop already exists in both target and source features
  if (any(colnames(target) == sid)) {
    colnames(target)[names(target) == sid] <- 'target_id'
  }

  if (any(colnames(target) == spop)) {
    colnames(target)[names(target) == spop] <- 'target_pop'
  }

  if (method == 'awi') {
    out <- pp_awi(target, source = source, sid = sid, spop = spop,
                  point = point)
  } else if (method == 'vwi') {
    if (volume == 'NULL') {
      usethis::ui_stop('volume is required for vwi')
    }

    if (!volume %in% colnames(target)) {
      usethis::ui_stop('{volume} cannot be found in the given target object')
    }

    if (!is.numeric(target[, volume, drop = TRUE])) {
      usethis::ui_stop('{volume} must be numeric')
    }

    out <- pp_vwi(target, source = source, sid = sid, spop = spop,
                  volume = volume, point = point)
  }

  return(out)
}
