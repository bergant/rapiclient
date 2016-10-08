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
  parameters <- setNames(vector("list", length(par_names)), par_names)

  f1 <- function() {
    #return(lapply(as.list(match.call())[-1], eval))

    # using formals() instead of match.call() to get the default values
    l1 <- as.list(mget(names(formals()), environment()))
    l1 <- l1[lapply(l1, mode) != "name"]
    return(l1[ !sapply(l1, is.null)])
  }

  formals(f1) <- do.call(alist, parameters)
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
