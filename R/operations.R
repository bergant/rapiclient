
# global constants for S3 classes
.class_api <- "rapi_api"
.class_operation <- "rapi_operation"
.class_schema <- "rapi_schema"
.class_schema_function <- "rapi_schema_function"

#' Get API
#'
#' Create API object from Swagger specification
#'
#' @param url Api url
#' @seealso See also \code{\link{get_operations}} and \code{\link{get_schemas}}
#' @return API object
#' @examples
#' \dontrun{
#' # create operation and schema functions
#' api <- get_api(api_url)
#' operations <- get_operations(api)
#' schemas <- get_schemas(api)
#' }
#' @export
get_api <- function(url) {
  api <- jsonlite::fromJSON(url)

  # swagger element is required
  if(is.null(api$swagger)) {
    warning("Missing Swagger Specification version")
  }
  # Info element is required
  if(is.null(api$info)) {
    warning("Missing Specification Info")
  }
  # If the host is not included, the host serving the documentation is to be
  # used (including the port).
  if(is.null(api$host)) {
    host <- httr::parse_url(url)$host
    if(!is.null(host)) {
      port <- httr::parse_url(url)$port
      if(!is.null(port)) {
        host <- paste0(host, ":", port)
      }
      api$host <- host
    }
  }
  # If basepath is not included, the API is served directly under the host
  if(is.null(api$basePath)) {
    api$basePath <- ""
  }
  # If the schemes is not included, the default scheme to be used is the one
  # used to access the Swagger definition itself.
  if(is.null(api$schemes)) {
    api$schemes <- httr::parse_url(url)$scheme
  }
  if(is.null(api$paths)) {
    warning("There is no paths element in the API specification")
  }

  class(api) <- c(.class_api, class(api))
  api
}

#' Simple functions to handle http response
#'
#' When creating operations from api one can define
#' how the response from http should be handled.
#' These functions can be used for simple result handling.
#'
#' See \code{\link{get_operations}} for details.
#'
#' @name result_handlers
#' @param x A response object from httr package (see \link[httr]{response}
#'   object in \pkg{httr} package  documentation)
#' @return Content of http response
#' @export
#' @examples
#' \dontrun{
#' operations <- get_operations(api, handle_response = content_or_stop)
#' }
content_or_stop <- function(x) {
  res <- httr::stop_for_status(x)
  if(inherits(res, "response")) {
    httr::content(res)
  } else {
    res
  }
}

#' @describeIn result_handlers Returns content or issues a warning
#' @export
content_or_warning <- function(x) {
  res <- httr::warn_for_status(x)
  if(inherits(res, "response")) {
    httr::content(res)
  } else {
    res
  }
}
#' @describeIn result_handlers Returns content or prints a message
#' @export
content_or_message <- function(x) {
  res <- httr::message_for_status(x)
  if(inherits(res, "response")) {
    httr::content(res)
  } else {
    res
  }
}

#' Get Operations Definitions
#'
#' A list of operations definitions organized by operationId field
#'
#' @param api API object
#' @param path (optional) filter by path
#' @export
#' @keywords internal
get_operation_definitions <- function(api, path = NULL) {
  ret <- list()
  path_names <- names(api$paths)
  if(!is.null(path)) {
    path_names <- path_names[grep(path, path_names)]
  }
  for(path in path_names) {
    action_types <- c("post", "get", "delete", "put")
    for(action in intersect(names(api$paths[[path]]), action_types)) {
      operation <- api$paths[[path]][[action]]
      operation$path <- path
      operation$action <- action
      if(is.null(operation$operationId)) {
        # sometimes there is no operationId?
        # example:
        #  (http://developer.nytimes.com/top_stories_v2.json/swagger.json)
        if(!is.null(operation$summary)) {
          operation$operationId <- gsub(" ", "_", operation$summary)
        } else {
          operation$operationId <- gsub("[{}]", "", operation$path)
          operation$operationId <- gsub("/", "_", operation$operationId)
          operation$operationId <- gsub("^_", "", operation$operationId)
        }
      }
      ret <- c(ret, stats::setNames(list(operation), operation$operationId))
    }
  }
  ret
}

#' Get parameter values
#'
#' Returns parameters, passed to a function with default parameters
#'
#' @param fun A function (to be passed to formals)
#' @param env environment
#' @return A named list of parameters
#' @keywords internal
get_par_values <- function(fun, env) {
  if(length(formals()) > 0 ) {
    l1 <- as.list(mget(names(formals(fun)), env))
    l1 <- l1[lapply(l1, mode) != "name"]
    x <- l1[ !vapply(l1, is.null, logical(1))]
  } else {
    x <- list()
  }
  x
}


#' Message body
#'
#' Transform a list to http request message body
#'
#' @param x A list
#' @keywords internal
get_message_body <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)

  if(getOption("rapiclient.log_request", default = FALSE)) {
    cat(json, "\n",
        file = file.path(
          getOption("rapiclient.log_request_path", "rapiclient_log.json")
        ), append = FALSE
    )
  }
  json
}


#' Get Operations
#'
#' Creates a list of functions from API operations definition. Names in a list
#' are operationIDs from API.
#'
#' All functions return a \link[httr]{response} object from httr package or a
#' value returned by \code{handle_response} function if specified. When
#' \code{path} is defined, only operations with the specified API path root are
#' created. Use \code{.headers} parameters to send additional headers when
#' sending a request.
#'
#' @section Handling response:
#'
#'   If no response handler function is defined, operation functions return
#'   \link[httr]{response} object (\pkg{httr} package). See httr
#'   \link[httr]{content} documentation for extracting content from a request,
#'   and functions \link[httr]{http_error} and \link[httr]{http_status} how to
#'   handle http errors and error messages.
#'
#'   When using simple \code{\link{result_handlers}}, operations will return the
#'   content of response instead of httr response object (or handle error as
#'   exception or warning in case of error).
#'
#'   To handle response automatically with custom function, define a function
#'   with httr response object as argument and pass it as \code{handle_response}
#'   argument to \code{get_operations} function.
#'
#' @param api API object (see \code{\link{get_api}})
#' @param .headers Optional headers passed to httr functions. See
#'   \code{\link[httr]{add_headers}} documentation
#' @param path (optional) filter by path from API specification
#' @param handle_response (optional) A function with a single argument: httr
#'   response
#' @return A list of functions.
#' @examples
#' \dontrun{
#' # create operation and schema functions
#' api <- get_api(api_url)
#' operations <- get_operations(api)
#' schemas <- get_schemas(api)
#'
#' # get operations which return content or stop on error
#' operations <- get_operations(api, handle_response = content_or_stop)
#'
#' # use .headers when operations must send additional heders when sending request
#' operations <- get_operations(api, .headers = c("api-key" = Sys.getenv("SOME_API_KEY"))
#' }
#' @export
get_operations <- function(api, .headers = NULL, path = NULL,
                           handle_response = identity) {

  operation_defs <- get_operation_definitions(api, path)

  lapply(operation_defs, function(op_def){

    # url
    get_url <- function(x) {

      operation_url <-
        paste0(api$schemes[1], "://", api$host, api$basePath, op_def$path)

      if(length(op_def$parameters)) {

        # parameters in path
        pars_in_path <-
          op_def$parameters[op_def$parameters$`in`=="path", "name"]
        for(p in pars_in_path) {
          if(!is.null(x[[p]])) {
            operation_url <- gsub(sprintf("\\{%s\\}", p), x[[p]], operation_url)
          }
        }

        # parameters in query
        params_in_query <-
          op_def$parameters[op_def$parameters$`in`=="query", "name"]
        url_query <- paste(
          unlist(
            lapply(params_in_query, function(par_name) {
              sprintf("%s=%s", par_name, x[[par_name]])
            })
          ),
          collapse = "&"
        )
        operation_url <- paste0(operation_url, "?", url_query)
      }
      url_ret <- httr::build_url(httr::parse_url(operation_url))
      return(url_ret)
    }


    # function body
    if(op_def$action == "post") {
      tmp_fun <- function() {
        x <- get_par_values(sys.function(sys.parent()), environment())
        request_json <- get_message_body(x)
        result <- httr::POST(
          url = get_url(x),
          body = request_json,
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
        handle_response(result)
      }
    } else if(op_def$action == "put") {
      tmp_fun <- function() {
        x <- get_par_values(sys.function(sys.parent()), environment())
        request_json <- get_message_body(x)
        result <- httr::PUT(
          url = get_url(x),
          body = request_json,
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
        handle_response(result)
      }
    } else if(op_def$action == "get") {
      tmp_fun <- function() {
        x <- get_par_values(sys.function(sys.parent()), environment())
        result <- httr::GET(
          url = get_url(x),
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
        handle_response(result)
      }
    } else if(op_def$action == "delete") {
      tmp_fun <- function() {
        x <- get_par_values(sys.function(sys.parent()), environment())

        result <- httr::DELETE(
          url = get_url(x),
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
        handle_response(result)
      }
    }

    # create function arguments from operation parameters definition
    parameters <- get_parameters(api, op_def$parameters)
    if(length(parameters)) {
      formals(tmp_fun) <- do.call(alist, parameters)
    }

    # add the complete operation definition as a functin attribute
    attr(tmp_fun, "definition") <- op_def
    class(tmp_fun) <- c(.class_operation, class(tmp_fun))
    tmp_fun
  })

}


#' Get Parameters
#'
#' Extract all parameters from parameters definition as a list
#' In case of reference to schema, use the schema.
#' @param parameters_def A parameters data frame from API operations  definition
#' @keywords internal
get_parameters <- function(api, parameters_def) {
  parameters <-
    lapply(parameters_def$name, function(par_name) {
      param <- parameters_def[parameters_def$name == par_name,]
      schema_ref <- param$schema$`$ref`
      if(!is.null(schema_ref) && !is.na(schema_ref)) {
        schema <- get_schema(api, schema_ref)
        par_name <- names(schema$properties)
      }
      par_name
    })

  if(length(parameters)) {
    parameters <- unlist(parameters)
    parameters <- stats::setNames(
      vector("list", length(parameters)),
      parameters
    )
  }
  parameters
}

