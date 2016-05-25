
# global constants for S3 classes
.class_api <- "rapi_api"
.class_operation <- "rapi_operation"
.class_schema <- "rapi_schema"

#' Get API
#'
#' Get swagger API from url
#'
#' @param url Api url
#' @return API object
#' @export
get_api <- function(url) {
  api <- jsonlite::fromJSON(url)
  class(api) <- c(.class_api, class(api))
  api
}


#' Get Operations Definitions
#'
#' A list of operations definitions organized by operationId field
#'
#' @param  api API object
#' @export
#' @keywords internal
get_operation_definitions <- function(api, tags = NULL) {
  ret <- list()
  for(path in names(api$paths)) {
    for(action in names(api$paths[[path]])) {
      operation <- api$paths[[path]][[action]]
      operation$path = path
      operation$action = action
      if(is.null(operation$operationId)) {
        # sometimes there is no operationId? (http://developer.nytimes.com/top_stories_v2.json/swagger.json)
        operation$operationId <- gsub(" ", "_", operation$summary)
      }
      ret <- c(ret, setNames(list(operation), operation$operationId))
    }
  }
  ret
}



#' Create Operation Functions
#'
#' Creates a list of functions from API operations definition.
#' Names in a list are operationIDs from API.
#'
#' @param api API object (see \code{\link{get_api}})
#' @param .headers Optional headers passed to \code{\link[httr]{add_headers}}
#' @return A list of functions.
#' @export
get_operations <- function(api, .headers = NULL) {

  operation_defs <- get_operation_definitions(api)

  lapply(operation_defs, function(op_def){

    # url
    get_url <- function(x) {

      operation_url <- paste0("http://", api$host, api$basePath, op_def$path)
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
      httr::build_url(httr::parse_url(operation_url))
    }

    # message body
    get_message_body <- function(x) {
      jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
    }

    # function body
    if(op_def$action == "post") {
      tmp_fun <- function() {
        x <- lapply(as.list(match.call())[-1], eval)
        request_json <- get_message_body(x)
        httr::POST(
          url = get_url(x),
          body = request_json,
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
      }
    } else if(op_def$action == "put") {
      tmp_fun <- function() {
        x <- lapply(as.list(match.call())[-1], eval)
        request_json <- get_message_body(x)
        httr::PUT(
          url = get_url(x),
          body = request_json,
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
      }
    } else if(op_def$action == "get") {
      tmp_fun <- function() {
        x <- lapply(as.list(match.call())[-1], eval)
        httr::GET(
          url = get_url(x),
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
      }
    } else if(op_def$action == "delete") {
      tmp_fun <- function() {
        x <- lapply(as.list(match.call())[-1], eval)
        httr::DELETE(
          url = get_url(x),
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
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
#' Exctract all parameters from parameters definition as a list
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
    parameters <- setNames(vector("list", length(parameters)), parameters)
  }
  return(parameters)

}

