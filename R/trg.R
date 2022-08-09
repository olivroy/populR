#' @title Target (trg)
#' @docType data
#' @description An object of \code{sf} class representing the buildings of a subset
#'     area of the city of Mytilini, Greece. The data set contains 179 building
#'     units along with the number of floors and residential use in binary format
#'     where 0 for non-residential floors and 1 for residential floors.
#' @format object of \code{sf} class with 179 rows and 12 columns:
#' \describe{
#'   \item{\code{tid}}{Target identification number}
#'   \item{\code{floors}}{Number of floors}
#'   \item{\code{rf}}{Reference population estimates}
#'   \item{\code{geometry}}{Geometry}
#'}
#' @source \url{http://mbatsaris.gr/}
"trg"
