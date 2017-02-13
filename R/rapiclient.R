#' rapiclient: Open API (Swagger) Client
#'
#' Create R functions directly from
#' OpenAPI (formerly Swagger) specification.
#'
#' @section Creating a client:
#'
#'   Use \code{\link{get_api}} to read the specification,
#'   \code{\link{get_operations}} to get client functions and
#'   \code{\link{get_schemas}} to create functions for additional schemas.
#'
#'   See usage example at \url{https://github.com/bergant/rapiclient#rapiclient}
#'
#'   Check out \url{https://github.com/OAI/OpenAPI-Specification} for additional
#'   information about Open API specification
#' @examples
#' \dontrun{
#' # Read API description
#' api <- get_api(api_url)
#'
#' # create operation and schema functions
#' operations <- get_operations(api)
#' schemas <- get_schemas(api)
#'
#' # call service
#' operations$some_operation(x, y, schemas$some_structure(u, v, ...))
#' }
#'
#' @section Support:
#'
#'   Please use \url{https://github.com/bergant/rapiclient/issues} for issues
#'
#' @docType package
#' @name rapiclient-package
#' @aliases rapiclient
NULL



