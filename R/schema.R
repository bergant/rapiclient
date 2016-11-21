get_schema <- function(api, ref) {
  if(!grepl("^#/definitions", ref )) {
    ref <- paste0("#/definitions/", ref)
  }
  ref_pos <- strsplit(ref, "/")[[1]]
  schema <- api[[ref_pos[[2]]]][[ref_pos[[3]]]]
  attr(schema, "name") <- ref_pos[[3]]
  class(schema) <- c(.class_schema, class(schema))
  schema
}



# Schema Function Wrapper
#
# Create function with parameters from schema
#
get_schema_function <- function(schema) {
  par_names <- names(schema$properties)
  parameters <- stats::setNames(vector("list", length(par_names)), par_names)

  f1 <- function() {
    #return(lapply(as.list(match.call())[-1], eval))

    # using formals() instead of match.call() to get the default values
    l1 <- as.list(mget(names(formals()), environment()))
    l1 <- l1[lapply(l1, mode) != "name"]
    return(l1[ !sapply(l1, is.null)])
  }

  formals(f1) <- do.call(alist, parameters)
  #attr(f1, "schema") <- schema
  class(f1) <- c(.class_schema_function, class(f1))
  f1
}

#' Get Schemas
#'
#' Returns a list of functions with arguments from API schemas.
#' Elements are named by schema names,
#' each function returns a named list.
#'
#' @param api Api object
#' @return A list of functions
#' @export
get_schemas <- function(api) {

  function_list <-
    lapply(names(api$definitions), function(schema_name) {
      schema <- get_schema(api, schema_name)
      get_schema_function(schema)
    })
  names(function_list) <- names(api$definitions)
  function_list
}


#' Set default arguments
#'
#' Use this functions to simplify operation and schema functions with default
#' arguments
#'
#' @param f function
#' @param ... Parameters with default values
#' @name default_arguments
#' @return A function with new defaults on arguments
#' @export
set_default_args <- function (f, ...) {
  args <- list(...)
  formals(f)[names(args)] <- args
  f
}

#' @param arguments A named list of arguments names and values
#' @rdname default_arguments
#' @export
set_default_args_list <- function (f, arguments) {
  formals(f)[names(arguments)] <- arguments
  f
}

#' @param f_call A function call
#' @rdname default_arguments
#' @export
#' @keywords internal
set_default_args_call <- function(f_call) {
  arguments <- as.list(substitute(f_call)[-1])
  f <- eval(substitute(f_call)[[1]])
  formals(f)[names(arguments)] <- arguments
  f
}
