context("External API test")

test_that("Reads external API operations", {

  # This test will skip when not in interactive mode
  # It reads api description from remote location.
  #
  # To run this test, use:
  #  devtools::test(filter = "*external*")

  if(!interactive()) {
    skip("Run only in interactive mode")
  }

  # parse api specification
  api_file <- system.file(
      "service", "cBioPortal", "api.json",
      package = "cBioPortalData", mustWork = TRUE
  )
  ext_api <- suppressWarnings({
      get_api(api_file)
  })

  # operations and schemas
  operations <- get_operations(ext_api)
  expect(length(operations) > 10, "Missing operations")

  expect(all(
    vapply(operations, function(x) inherits(x, "rapi_operation"), logical(1))
  ), "Not all operations are rapi_operation class")

  expect(all(
    vapply(operations, function(x) typeof(x) == "closure", logical(1))
  ), "Not all operations are functions")

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

test_that("Reads remote and local yaml", {
    yaml_fl <- system.file("extdata/sample_specs/petstore.yaml",
        package = "rapiclient", mustWork = TRUE)
    local_api <- get_api(yaml_fl)

    expect_true(
        inherits(local_api, "rapi_api")
    )

    if (!interactive()) {
      skip("Run only in interactive mode")
    }
    yaml <- paste0(
        "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/",
        "examples/v2.0/yaml/petstore.yaml"
    )
    ext_api <- get_api(yaml)
    yaml_fl <- tempfile(fileext=".yaml")
    download.file(yaml, yaml_fl)
    local_api <- get_api(yaml_fl)
    expect_identical(
        ## ext_api has host, schemes
        ext_api[names(local_api)], local_api[]
    )
})
