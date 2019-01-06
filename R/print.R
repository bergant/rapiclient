#' Print Operation
#'
#' @param text character(1) to be formatted to the width of the user
#'   screen, with additional arguments \code{...} passed to
#'   \code{strwrap()}.
#' @keywords internal
print_wrap <- function(text, ...) {
  paste(strwrap(text, ...), collapse="\n")
}

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
  cat(print_wrap(x$info$description), "\n\n")
  op_defs <- get_operation_definitions(x)

  for(op_def in op_defs) {
    summary <- print_wrap(op_def$summary, indent = 4, exdent = 4)
    cat(op_def$operationId, "\n", summary, "\n", sep="")
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
    description <- print_wrap(op_def$description, indent = 2, exdent =2)
    cat("Description:\n", description, "\n", sep="")
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
      description <-
        print_wrap(p$description, indent = 4, exdent = 4)
      cat(description, "\n", sep="")
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
        description <-
          print_wrap(nodes[[n]]$description[[p]], indent = 4, exdent = 4)
        cat(description, "\n", sep="")
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
  text <- paste0(
    attr(x, "schema_name"),
    "(", paste(names(formals(x)), collapse=", "), ")"
  )
  cat(print_wrap(text, indent = 2, exdent = 4), "\n", sep="")
}


