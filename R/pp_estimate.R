#' Areal Interpolation of Population Data
#'
#' @param target An object of class \code{sf} that is used to interpolate data
#'     to. Usually, target may include polygon features representing building units
#' @param source An object of class \code{sf} including data to be interpolated.
#'     Source may be a set of coarse polygon features such as city blocks or
#'     census tracts
#' @param sid Source identification number
#' @param spop Source population values to be interpolated
#' @param volume Target feature volume information (height or number of floors).
#'     Required when \code{method=vwi}
#' @param point Whether to return point geometries (FALSE by default)
#' @param method Two methods provided: \code{awi} (areal weighting interpolation)
#'     and \code{vwi} (volume weighting interpolation). \code{awi} proportionately
#'     interpolates the population values based on areal weights calculated
#'     by the area of intersection between the source and target zones. \code{vwi}
#'     proportionately interpolates the population values based on areal weights
#'     calculated by the area of intersection between the source and target zones
#'     multipled by the volume information (height or number of floors).
#'
#' @return An object of class \code{sf} including estimated population
#'     counts for target features using either \code{awi} or \code{vwi}
#'     methods. The estimated population counts are stored in a new column called
#'     pp_est.
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
    usethis::ui_stop('{sid} cannot be found')
  }

  if (!spop %in% colnames(source)) {
    usethis::ui_stop('{spop} cannot be found')
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
    usethis::ui_stop('point must be logical (T|TRUE, F|FALSE')
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
      usethis::ui_stop('{volume} cannot be found')
    }

    if (!is.numeric(target[, volume, drop = TRUE])) {
      usethis::ui_stop('{volume} must be numeric')
    }

    out <- pp_vwi(target, source = source, sid = sid, spop = spop,
                  volume = volume, point = point)
  }

  return(out)
}
