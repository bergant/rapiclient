#library(testthat)

context("External API test")

test_that("Reads external API operations", {

  # execute this test from console with:
  #  devtools::test(filter = "*external*")

  if(!interactive()) skip("Only in interactive mode")

  # parse api specification
  ext_api <- get_api("http://api.opentrials.net/v1/swagger.yaml")

  # operations and schemas
  operations <- get_operations(ext_api)
  expect(length(operations) > 10, "Missing operations")

  expect(all(
    vapply(operations, function(x) inherits(x, "rapi_operation"), logical(1))
  ), "Not all operations are rapi_operation class")

  expect(all(
    vapply(operations, function(x) typeof(x) == "closure", logical(1))
  ), "Not all operations are functions")

  #res <- operations$autocomplete(`in` = "condition")
  #httr::http_status(res)

  # schemas
  schemas <- ext_api[["definitions"]]
  s_struct <- lapply(schemas, function(schema) {
    get_schema_structure(ext_api, schema)
  })
  expect(
    all(
      vapply(s_struct,
             function(x) identical(names(x), c("nodes", "edges")),
             logical(1))
    ),
    "Expected nodes and edges in schema structure"
  )


  gv_dots <-
    vapply(names(schemas), function(schema_name) {
      schema <- schemas[[schema_name]]
      gr_dot <- get_schema_graphviz_dot(ext_api, schema, schema_name)
      #print(DiagrammeR::grViz(gr_dot))
      gr_dot
    }, FUN.VALUE = character(1))

  expect(
    all(
      vapply(gv_dots, function(x) grepl("digraph", x),FUN.VALUE = logical(1))
    ),
    "Missing 'digraph' keyword in some dot string"
  )
})
