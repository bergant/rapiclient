#' Print API object
#'
#' @param x API object
#' @param ...further arguments passed to or from other methods
#' @export
#' @keywords internal
print.rapi_api <- function(x, ...) {
  cat("Swagger API", paste(x$swagger, x$info$title, x$info$version))
  cat("\n")
  cat(x$host, x$basePath, sep = "")
  cat("\n")

  cat("\n")
  cat(x$info$description, "\n\n")
  op_defs <- get_operation_definitions(x)

  for(op_def in op_defs) {
    cat(op_def$operationId, "\n  ", op_def$summary, "\n")
  }

}


print_schema <- function(api, x, name = NULL) {
  properties <- x$properties
  p_name <- names(properties)[5]
  schema_refs <- c()
  if(!is.null(name)) cat(name, "\n")
  for(p_name in names(properties)) {
    p <- properties[[p_name]]
    type <- properties[[p_name]]$type
    if(!is.null(p$`$ref`)) {
      type <- gsub("^#/definitions/","",p$`$ref`)
      schema_refs <- c(schema_refs, p$`$ref`)
    }
    if(type == "array" ) {
      if(!is.null(properties[[p_name]]$items$type)) {

        type <- paste0(type, "[", properties[[p_name]]$items$type, "]")
      }
      if(!is.null(properties[[p_name]]$items$`$ref`)) {
        subtype <- gsub("^#/definitions/","", properties[[p_name]]$items$`$ref`)
        type <- paste0(type, "[", subtype, "]")
        schema_refs <- c(schema_refs, properties[[p_name]]$items$`$ref`)
      }
    }
    cat("  ", p_name, " (", type, ")\n", sep = "" )
  }
  schema_refs <- unique(schema_refs)
  schemas <- lapply(schema_refs, function(x) get_schema(api, x))
  names(schemas) <- schema_refs
  for(schema in names(schemas)) {
    print_schema(api, schemas[[schema]], gsub("^#/definitions/","", schema))
  }
}


#' Print Operation
#'
#' @param x Operation
#' @param ...further arguments passed to or from other methods
#' @export
#' @keywords internal
print.rapi_operation <- function(x, ...) {

  #  operation <- operations$createUsersWithArrayInput
  if(!inherits(x, c(.class_operation))) {
    stop("Not an operation object.")
  }

  op_def <- attr(x, "definition")
  api <- get("api", parent.env(environment(fun = x)))

  cat(op_def$operationId, "\n")
  cat(op_def$summary, "\n")
  if(!is.null(op_def$description) && op_def$description != "") {
    cat("Description:\n  ", op_def$description, "\n")
  }

  if(length(op_def$parameters)) {

    cat("\n")
    cat("Parameters:\n")
    for(i in seq_len(nrow(op_def$parameters))) {
      if(!is.null(op_def$parameters[i,]$schema$`$ref`) &&
         !is.na(  op_def$parameters[i,]$schema$`$ref`)) {
        schema_name <- gsub("^#/definitions/","", op_def$parameters[i,"schema"]$`$ref`)
        #cat("(from", schema_name, "schema:)\n")
        print_schema(api, get_schema(api, op_def$parameters[i,"schema"]$`$ref`))
      } else if(!is.null(op_def$parameters[i,]$schema$items$`$ref`)) {
        schema_name <- gsub("^#/definitions/","", op_def$parameters[i,"schema"]$items$`$ref`)
        cat("Array of", schema_name, ":\n")
        print_schema(api, get_schema(api, op_def$parameters[i,"schema"]$items$`$ref`))
      } else {
        pars <- op_def$parameters[i, ]
        type <- pars$type
        subtype <- pars$items$type
        if(!is.null(subtype)) {
          type <- paste0(type, "[", subtype, "]")
        }

        cat("  ", pars$name, " (", type, ")\n    ", pars$description, sep = "")
      }
      cat("\n")
    }
  }

}
