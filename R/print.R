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

  # there is an api object in the environment of the operation function
  api <- get("api", parent.env(environment(fun = x)))

  cat(op_def$operationId, "\n")
  cat(op_def$summary, "\n")
  if(!is.null(op_def$description) && op_def$description != "") {
    cat("Description:\n  ", op_def$description, "\n")
  }

  cat("\n")
  cat("Parameters:\n")

  for(p in op_def$parameters) {
    type <- p$type
    if(!is.null(p$`$ref`) ) {
      type <- gsub("#/definitions/", "", p$`$ref`)
    }
    subtype <- p$items$type
    if(!is.null(p$items$`$ref`)) {
      subtype <- gsub("#/definitions/", "", p$items$`$ref`)
    }
    if(!is.null(subtype)) {
      type <- paste0(type, "[", subtype, "]")
    }
    cat("  ", p$name, " (", type, ")\n", sep = "")
    if(!is.null(p$description)) {
      cat("    ", p$description, "\n", sep = "")
    }
  }
  #op_def <- attr(op1$addPet, "definition")
  schemas <- union(
    unlist(lapply(op_def$parameters, function(x) x[["$ref"]])),
    unlist(lapply(op_def$parameters, function(x) x$items[["$ref"]]))
  )
  schemas <- gsub("#/definitions/","", schemas)


  ss <- lapply(schemas, function(s) {
    get_schema_structure(api, api$definitions[[s]], name = s)
  })

  nodes <- unlist(lapply(ss, function(s) s$nodes), recursive = FALSE)
  nodes <- nodes[!duplicated(names(nodes))]

  for(n in names(nodes)) {
    cat(n, "\n")
    for(p in seq_len(nrow(nodes[[n]]))) {
      cat("  ", nodes[[n]]$name[p], " (", nodes[[n]]$type[p], ")\n", sep = "")
      if(nodes[[n]]$description[p]!="") {
        cat("    ", nodes[[n]]$description[p], "\n")
      }
    }
  }


}

#' Print Operation
#'
#' @param x Operation
#' @param ...further arguments passed to or from other methods
#' @export
#' @keywords internal
print.rapi_schema_function <- function(x, ...) {
  cat(
    "    ",
    attr(x, "schema_name"),
    "(",
    paste(names(formals(x)), collapse = ", "),
    ")",
    sep = ""
  )
}


