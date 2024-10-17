
# global constants for S3 classes
.class_api <- "rapi_api"
.class_operation <- "rapi_operation"
.class_schema <- "rapi_schema"
.class_schema_function <- "rapi_schema_function"

maybe_add_user_agent <- function(.headers) {
  if (is.null(.headers) || !"User-Agent" %in% names(.headers)) {
    c("User-Agent" = "https://github.com/bergant/rapiclient", .headers)
  } else {
    .headers
  }
}

fetch_content <- function(url, config = NULL) {
    if (!startsWith(url, "http") && file.exists(url))
        return(readLines(url, encoding = "UTF-8"))
    response <- httr::GET(url, config = config)
    httr::stop_for_status(response, "fetch_content_url")
    httr::content(response, as = "text", encoding = "UTF-8")
}

read_api_yaml <- function(apitext) {
    yaml::yaml.load(apitext)
}

read_api_json <- function(apitext) {
    jsonlite::fromJSON(apitext, simplifyDataFrame = FALSE)
}

#' Get API
#'
#' Create API object from Swagger specification
#'
#' @param url API URL or file (can be json or yaml format)
#'
#' @param config httr::config() curl options.
#'
#' @param ext the file extension of the API file (either 'yaml' or 'json'). By
#'   default, it is obtained from the URL with `tools::file_ext` and should
#'   be provided when the file URL is missing an extension.
#'
#' @seealso See also \code{\link{get_operations}} and \code{\link{get_schemas}}
#'
#' @return API object
#'
#' @examples
#' \dontrun{
#' # create operation and schema functions
#' api_url <- "http://petstore.swagger.io/v2/swagger.json"
#' api <- get_api(api_url)
#' operations <- get_operations(api)
#' schemas <- get_schemas(api)
#' }
#' @export
get_api <- function(url, config = NULL, ext) {
    if (missing(ext))
        ext <- tolower(tools::file_ext(url))
    FUN <- switch(
        ext,
        yml =,
        yaml = read_api_yaml,
        json = read_api_json,
        stop(
            "'url' does not appear to be JSON or YAML.",
            " If format is known, provide 'ext' as input."
        )
    )
    apitext <- fetch_content(url, config = config)
    api <- FUN(apitext)

  # swagger element is required
  if (is.null(api$swagger)) {
    warning("Missing Swagger Specification version")
  }
  # Info element is required
  if(is.null(api$info)) {
    warning("Missing Specification Info")
  }
  # If the host is not included, the host serving the documentation is to be
  # used (including the port).
  if(is.null(api$host)) {
    host <- httr::parse_url(url)$hostname
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

  # remove the trailing "/" from base path
  api$basePath <- gsub("/$", "", api$basePath)

  # If the schemes element  is not included, the default scheme to be used is
  # the one used to access the Swagger definition itself.
  if(is.null(api$schemes)) {
    api$schemes <- httr::parse_url(url)$scheme
  }
  if(is.null(api$paths)) {
    warning("There is no paths element in the API specification")
  }

  if (!(is.null(config) || inherits(config, "request")))
    stop("'config' must be NULL or an instance of httr::config()")
  api$config <- config

  class(api) <- c(.class_api, class(api))
  api
}

.get_names <- function(list) {
  vapply(list, `[[`, character(1L), "name")
}

#' Get Operations Definitions
#'
#' Get a list of operations definitions from API specification
#'
#' Operations are parsed from `paths`` element for every path and every action
#' inside path. Operation name is set to `operationId` from each action.
#'
#' See also specification \url{https://swagger.io/specification/#operationObject}
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
  for(path_name in path_names) {
    action_types <-
      c("post", "patch", "get", "head", "delete", "put")
    # parameters may be defined on the path level

    for(action in intersect(names(api$paths[[path_name]]), action_types)) {

      operation <- api$paths[[path_name]][[action]]

      operation$path <- path_name
      operation$action <- action

      # parameters can be defined on path level and overridden on operation
      # level
      #
      # Note that parameters is often a list() rather than NULL, so deal
      # that situation as well.
      if(is.null(operation$parameters) || length(operation$parameters)==0) {
        operation$parameters <- api$paths[[path_name]]$parameters
      } else {
        ## check names in operations parameters
        name_o_params <- .get_names(operation$parameters)
        name_api_params <- .get_names(api$paths[[path_name]]$parameters)
        keep_idx <- !duplicated(c(name_o_params, name_api_params))
        operation$parameters <-
          c(operation$parameters, api$paths[[path_name]]$parameters)[keep_idx]
      }

      # get referenced parameters (when parameter has $ref = #/parameters/...)
      operation$parameters <-
        lapply(operation$parameters, function(p) {
          ref <- p[["$ref"]]
          if(!is.null(ref) && grepl("#/parameters", ref)) {
            api$parameters[[gsub("#/parameters/", "", ref)]]
          } else {
            p
          }
        })

      # combine parameters defined in schema
      is_def_by_schema <-
        vapply(operation$parameters,
               function(x) !is.null(x$schema[["$ref"]]), logical(1))

      operation$parameters <-
        c(
          operation$parameters[!is_def_by_schema],
          unlist(
            recursive = FALSE,
            lapply(operation$parameters[is_def_by_schema], function(x) {
              get_parameters_from_schema(api, x$schema$`$ref`)
            })
          )
        )


      # It is possible that operationId is missing
      # example:
      #  (http://developer.nytimes.com/top_stories_v2.json/swagger.json)
      if(is.null(operation$operationId)) {
        if(!is.null(operation$summary)) {
          operation$operationId <- gsub(" ", "_", operation$summary)
        } else {
          operation$operationId <- gsub("[{}]", "", operation$path)
          operation$operationId <- gsub("/", "_", operation$operationId)
          operation$operationId <- gsub("^_", "", operation$operationId)
        }
      }
      ret <- c(ret, structure(list(operation), .Names = operation$operationId))
    }
  }
  names(ret) <- trimws(names(ret))
  ret
}




#' Get operations
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
#'
#' @param .headers Optional headers passed to httr functions. See
#'   \code{\link[httr]{add_headers}} documentation
#'
#' @param path (optional) filter by path from API specification
#'
#' @param handle_response (optional) A function with a single argument: httr
#'   response
#'
#' @inheritParams jsonlite::toJSON
#'
#' @return A list of functions.
#'
#' @examples
#' \dontrun{
#' # create operation and schema functions
#' api_url <- "http://petstore.swagger.io/v2/swagger.json"
#' api <- get_api(api_url)
#' operations <- get_operations(api)
#' schemas <- get_schemas(api)
#'
#' # get operations which return content or stop on error
#' operations <- get_operations(api, handle_response = content_or_stop)
#'
#' # use .headers when operations must send additional headers when sending
#' operations <-
#'   get_operations(api, .headers = c("api-key" = Sys.getenv("SOME_API_KEY")))
#' }
#' @export
get_operations <-
    function(
        api, .headers = NULL, path = NULL,
        handle_response = identity, auto_unbox = FALSE
    )
{
    operation_defs <- get_operation_definitions(api, path)
    lapply(operation_defs, function(op_def) {
        what <- toupper(op_def$action)
        if (!what %in% c("POST", "PATCH", "PUT", "GET", "HEAD", "DELETE"))
            stop("unsupported REST operation '", what, "'")
        HTTR_FUN <- get(what, envir = getNamespace("httr"))
        FUN <- switch(
            what,
            POST =,
            PATCH =,
            PUT = function(..., .__body__ = list()) {
                args <- .api_args(formals(), environment())
                body0 <- .api_body(formals(), ..., .__body__ = .__body__)
                body <-  get_message_body(op_def, body0, auto_unbox)
                result <- HTTR_FUN(
                    url = .get_url(api, op_def, args),
                    config = .get_config(api),
                    .get_content_type(op_def),
                    .get_accept(op_def),
                    httr::add_headers(.headers = maybe_add_user_agent(.headers)),
                    body = body
                )
                handle_response(result)
            },
            GET =,
            HEAD =,
            DELETE = function(...) {
                args <- .api_args(formals(), environment())
                result <- HTTR_FUN(
                    url = .get_url(api, op_def, args),
                    config = .get_config(api),
                    .get_content_type(op_def),
                    .get_accept(op_def),
                    httr::add_headers(.headers = maybe_add_user_agent(.headers))
                )
                handle_response(result)
            }
        )

        ## create function arguments from operation parameters definition
        parameters <- get_parameters(api, op_def$parameters)
        idx <- .api_is_message_body_parameter(op_def, parameters)
        args <- do.call("alist", parameters[!idx])
        if (what %in% c("POST", "PATCH", "PUT")) {
            alist0 <- do.call("alist", list(.__body__ = parameters[idx]))
            args <- c(args, alist(...=), alist0)
        }
        formals(FUN) <- args

        ## add the complete operation definition as a function attribute
        attr(FUN, "definition") <- op_def
        class(FUN) <- c(.class_operation, class(FUN))
        FUN
    })
}

#' Message body
#'
#' Transform a list of operation arguments to an http request message
#' body. This method searches for parameters with swagger / openAPI
#' specification `in: body` or `in: formData`. `body` parameters are
#' expected to be R vectors or lists, and are transformed to JSON
#' using `jsonlite::toJSON()`. `formData` parameters are treated as
#' is, so must be specified (e.g., using `httr::upload_file()`) by the
#' caller. Interpretation of `formData` parameters require that the
#' `op_def` includes `consumes: multipart/form-data`.
#'
#' @param op_def A list representation of the swagger / openAPI
#'     description of the operation.
#'
#' @param body A list representation of the operation arguments provided
#'     by the user.
#'
#' @inheritParams jsonlite::toJSON
#'
#' @return A JSON character representation (for `body`) or list of
#'     objects (for `formData`) representing the parameters `x`.
#'
#' @keywords internal
get_message_body <- function(op_def, body, auto_unbox = TRUE) {
    if (identical(op_def$consumes, "multipart/form-data")) {
        json <- body
    } else {
        ## unbox?
        name <- vapply(op_def$parameters, `[[`, character(1), "name")
        ## match body names first
        body <- body[names(body) %in% name]
        type <- vapply(op_def$parameters, function(elt) {
            type <- elt$type
            if (is.null(type)) {
                ## FIXME: recurse into $ref
                NA_character_
            } else {
                type
            }
        }, character(1))
        if (identical(length(body), 1L))
            body <- body[[1L]]
        if (is.null(body) || all(is.na(body)) || !length(body)) {
            json <- structure("{}", class = "json")
        } else {
            for (nm in names(body)) {
                idx <- match(nm, name)
                if (type[idx] %in% c("string", "number", "integer", "boolean"))
                    body[[nm]] <- jsonlite::unbox(body[[nm]])
            }
            json <- jsonlite::toJSON(
                body, pretty = TRUE, auto_unbox = auto_unbox
            )
          }
    }
    if (getOption("rapiclient.log_request", default = FALSE)) {
        cat(
          json, "\n",
          file = file.path(
              getOption("rapiclient.log_request_path", "rapiclient_log.json")
          ), append = FALSE
        )
    }
    json
}

#' Build operations url
#'
#' Build operations operation url for specified parameter values
#'
#' @param scheme http or https
#' @param host host name with port (delimited by ":")
#' @param base_path base path, defined in api specification
#' @param op_def a single operation definition
#' @param par_values parameter values in a list
#' @seealso \code{\link{get_operation_definitions}}
#' @keywords internal
build_op_url <- function(api, scheme, host, base_path, op_def, par_values) {
  path <- op_def$path
  parameters <- op_def$parameters
  query <- NULL

  if(length(parameters)) {
    par_location <- lapply(parameters, function(x) x$`in`)
    par_name <- lapply(parameters, function(x) x$name)
    if(length(unlist(par_location)) != length(par_location)) {
      stop("Not all parameters have a location")
    }
    if(length(unlist(par_name)) != length(par_name)) {
      stop("Not all parameters have a name")
    }

    # Change path with parameter values (path templating)
    # For example in /pet/{petId} the petId should be replaced with a value
    #   see specicifation http://swagger.io/specification/#pathTemplating
    for(p in parameters[par_location == "path"]) {
      if(!is.null(par_values[[p$name]])) {
        path <- gsub(sprintf("\\{%s\\}", p$name), par_values[[p$name]], path)
      }
    }

    # Parameters that are appended to the URL.
    # For example url should be /items?id=### when id location is query
    if(any(par_location=="query")) {
      query <- par_values[unlist(par_name[par_location == "query"])]
      query <- query[!vapply(query, is.null, logical(1))]
    }
  }
  # build url
  httr::modify_url(
    url =
      httr::parse_url(
        paste0(api$schemes[1], "://", api$host, api$basePath, path )
      ),
    query = query
  )
}

#' Get Parameters
#'
#' Extract all parameters from parameters definition as a list
#' In case of reference to schema, use the schema.
#' @param api API definition
#' @keywords internal
get_parameters <- function(api, api_parameters) {
  parameters <- get_parameters_definition(api, api_parameters)

  if(length(parameters)) {
    parameters <- unlist(parameters)
    parameters <- structure(
      vector("list", length(parameters)), .Names = parameters
    )
  }
  parameters
}

get_parameters_definition <- function(api, api_parameters) {

  lapply(api_parameters, function(p) {
    schema_ref <- p$schema$`$ref`
    if(!is.null(schema_ref) && !is.na(schema_ref)) {
      schema <- get_schema(api, schema_ref, compose_allOf = TRUE)
      par_name <- names(schema$properties)
    } else {
      par_name <- p$name
    }
  })
}

get_parameters_from_schema <- function(api, schema) {
  schema <- get_schema(api, schema, compose_allOf = TRUE)
  lapply(names(schema$properties), function(p_name) {
    sch_prop <- schema$properties[[p_name]]
    p <- list(
      name = p_name,
      `in` = "body",
      type = sch_prop$type,
      description = sch_prop$description
    )
    if(!is.null(sch_prop[["$ref"]]))  p$`$ref`<- sch_prop[["$ref"]]
    if(!is.null(sch_prop[["items"]])) p$items <- sch_prop[["items"]]
    p
  })
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
#'
#' @param x A response object from httr package (see \link[httr]{response}
#'   object in \pkg{httr} package  documentation)
#'
#' @return Content of http response
#'
#' @examples
#' api_file <- system.file(
#'   "extdata", "sample_specs", "petstore.yaml",
#'   package = "rapiclient", mustWork = TRUE
#' )
#' api <- get_api(api_file)
#' operations <- get_operations(api, handle_response = content_or_stop)
#'
#' @export
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

