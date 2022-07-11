#' RMSE
#'
#' @param target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source population
#' @param tpop target population
#' @param title scatterplot title \code{string}
#'
#' @return a list including rmse, mae, linear model details and correlation coefficient
#' @export
#'
#' @importFrom sf st_as_sf
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom Metrics rmse
#' @importFrom Metrics mae
#' @importFrom usethis ui_stop
#'
#' @examples
#' # read lib data
#' data('src')
#' data('target')
#'
#' # areal weighting interpolation - awi
#' awi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi)
#'
#' # volume weighting interpolation - vwi
#' vwi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # awi - rmse
#' pp_rmse(awi, src, sid = sid, spop = pop, tpop = pp_est,
#'     title ='awi')
#'
#' # vwi - rmse
#' pp_rmse(vwi, src, sid = sid, spop = pop, tpop = pp_est,
#'     title ='vwi')
#'
pp_rmse <- function(target, source, sid, spop, tpop, title) {
  # check arguments
  if (missing(source)) {
    usethis::ui_stop('source is required')
  }

  if (missing(target)) {
    usethis::ui_stop('target is required')
  }

  if (missing(sid)) {
    usethis::ui_stop('sid is required')
  }

  if (missing(spop)) {
    usethis::ui_stop('spop is required')
  }

  if (missing(tpop)) {
    usethis::ui_stop('tpop is required')
  }

  if (missing(title)) {
    usethis::ui_stop('title is required')
  }

  # check whether column names exist
  spop <- rlang::quo_name(rlang::enquo(spop))
  sid <- rlang::quo_name(rlang::enquo(sid))
  tpop <- rlang::quo_name(rlang::enquo(tpop))
  title <- rlang::quo_name(rlang::enquo(title))

  if (!spop %in% colnames(source)) {
    usethis::ui_stop('{spop} cannot be found in the given source object')
  }

  if (!sid %in% colnames(source)) {
    usethis::ui_stop('{sid} cannot be found in the given source object')
  }

  if (!tpop %in% colnames(target)) {
    usethis::ui_stop('{tpop} cannot be found in the given target object')
  }

  # check whether spop and tpop are numeric
  if (!is.numeric(source[, spop, drop = TRUE])) {
    usethis::ui_stop('{spop} must be numeric')
  }

  if (!is.numeric(target[, tpop, drop = TRUE])) {
    usethis::ui_stop('{tpop} must be numeric')
  }

  target <- st_as_sf(as.data.frame(target))
  source <- st_as_sf(as.data.frame(source))

  df <- source[, c(sid, spop), drop = TRUE]
  df[, tpop] <- 0

  # sumup target pop for each source zone feature
  for (i in 1:nrow(df)) {
    df[,tpop][i] <- sum(target[, tpop, drop = TRUE][target[, sid, drop = TRUE] == df[, sid][i]])
  }

  # calculate rmse, calculate correlation coefficient and create linear regression model
  rmse <- rmse(df[, tpop], df[, spop])
  mae <- mae(df[, tpop], df[, spop])
  linear_model <- lm(df[, tpop] ~ df[, spop])
  correlation_coef <- round(cor(df[, spop], df[, tpop]), 5)
  myList <- list(rmse = rmse, mae = mae, linear_model = linear_model, correlation_coef = correlation_coef)

  # scatterplot with line and correlation coeficient as text
  plot(df[, spop], df[, tpop], col="#634B56", main = title, xlab = "Observed", ylab = "Estimated")
  abline(linear_model, col="#FD8D3C")
  text(x = min(df[, spop]) + 40, y = max(df[, tpop]) - 20, label = paste0("r^2 = ", correlation_coef))

  return(myList)

}
