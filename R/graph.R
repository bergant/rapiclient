#' Get schema structure
#'
#' (experimental) Produces nodes and edges where nodes are a list of schemas, represented as
#' data frames with schema properties and edges are relations between nested
#' schemas
#'
#' @param api Api object
#' @param x A schema definition from api (a list object)
#' @param name A name of root schema
#' @export
#' @keywords internal
get_schema_structure <- function(api, x, name = NULL) {
  if(is.null(name)) {
    name <- tail(as.character(substitute(x)),1)
  }

  schema_struct <- list(
    nodes = list(),
    edges = list()
  )

  schema_walk <- function(api, x, name = "root") {
    schema_refs <- c()
    i <- length(schema_struct$nodes) + 1
    schema_struct$nodes[[i]] <<- list(name = name, properties = list())
    properties <- x$properties
    if(!is.null(x$allOf)) {
      #properties <- x$allOf$properties[x$allOf$properties]
      if(!is.null(x$allOf$`$ref`)) {
        properties <- lapply(x$allOf$properties, function(x) list(type = x$type[!is.na(x$type)]))
        schema_allof <- x$allOf$`$ref`[!is.na(x$allOf$`$ref`)]
        schema_refs <- c(schema_refs, schema_allof )
        properties$`*` <- list(type = basename(schema_allof))
      }
    }
    if(x$type == "array") {
      if(!is.null(x$items$`$ref`)) {
        schema_refs <- c(schema_refs, x$items$`$ref`)
      }
    }
    for(p_name in names(properties)) {
      p <- properties[[p_name]]
      type <- properties[[p_name]]$type
      description <- properties[[p_name]]$description
      if(is.null(description)) description <- NA
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
      tmp_node <- list(name = p_name, type = type, description = description)
      schema_struct$nodes[[i]][["properties"]][[p_name]] <<- tmp_node

      #cat("  ", p_name, " (", type, ")\n", sep = "" )
    }
    schema_refs <- unique(schema_refs)
    if(length(schema_refs > 0)) {
      schema_struct$edges <<- unique(rbind(
        schema_struct$edges,
        data.frame(from = name, to = basename(schema_refs), stringsAsFactors = FALSE)))
    }
    walked <- sprintf("#/definitions/%s",sapply(schema_struct$nodes, getElement, "name"))
    schemas <- lapply(schema_refs, function(x) get_schema(api, x))
    names(schemas) <- schema_refs
    for(schema in setdiff(names(schemas), walked)) {
      schema_walk(api, schemas[[schema]], gsub("^#/definitions/","", schema))
    }
  }

  schema_walk(api = api, x = x, name = name)

  node_names <- sapply(schema_struct$nodes, getElement, "name")

  schema_struct$nodes <-
    lapply(schema_struct$nodes, function(x) {
      data.frame(
        name = sapply(x$properties, function(p) p$name),
        type = sapply(x$properties, function(p) p$type),
        description = sapply(x$properties, function(p) p$description),
        stringsAsFactors = FALSE
      )
    })
  names(schema_struct$nodes) <- node_names
  schema_struct
}

#' Get schema graphviz dot
#'
#' Create a graphviz presentation of schema structure (experimental)
#'
#' @param api Api object
#' @param schema A schema definition from api (a list object)
#' @param schema_name A name of root schema
#' @export
#' @keywords internal
get_schema_graphviz_dot <- function(api, schema, schema_name = NULL, rankdir = "LR", gv_attrs = "") {
  if(is.null(schema_name)) {
    schema_name <- tail(as.character(substitute(schema)), 1)
  }

  ss <- get_schema_structure(api, schema, schema_name)

  sprintf(
    'digraph schema {\nrankdir="%s"\n%s\n\n%s\n\n%s}',
    rankdir,
    gv_attrs,
    paste0(
      sapply(names(ss$nodes), function(n) {
        paste0(
          '"',n,'"', '[shape = "Mrecord", label="',
          n, "|",
          paste0(
            sprintf('%s (%s)\\l', ss$nodes[[n]]$name, ss$nodes[[n]]$type),
            collapse = ""
          ),
          '"]\n',
          collapse = ""
        )
      }),
      collapse = "\n"
    ),
    paste(
      sprintf('"%s"->"%s"\n', ss$edges$from, ss$edges$to),
      collapse = ""
    )
  )

}




