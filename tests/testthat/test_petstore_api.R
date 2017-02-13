library(testthat)

context("Petstore sample API operations")

test_that("Reads API operations", {
  petstore_spec <-
    system.file("extdata/sample_specs", "petstore.json", package = "rapiclient")
  pet_api <- get_api(petstore_spec)
  operations <- get_operations(pet_api)
  for(operation in operations) {
    expect_type(operation, "closure")
    expect_is(operation, "rapi_operation")
  }
  expect_length(operations, 20)
  expect_output(print(operations))
  expect_output(print(pet_api))
})


context("Petstore sample API schemas")

test_that("Reads API schemas", {
  petstore_spec <-
    system.file("extdata/sample_specs/petstore.json", package = "rapiclient")
  pet_api <- get_api(petstore_spec)
  schemas <- get_schemas(pet_api)
  for(schema in schemas) {
    expect_type(schema, "closure")
    expect_is(schema, "rapi_schema_function")
  }
  expect_output(print(schemas))
})

context("Petstore sample API schemas structure")
test_that("Reads API schema structure", {
  petstore_spec <-
    system.file("extdata/sample_specs/petstore.json", package = "rapiclient")
  pet_api <- get_api(petstore_spec)
  schemas <- pet_api[["definitions"]]
  for(schema in schemas) {
    ss <- get_schema_structure(pet_api, schema)
    expect("nodes" %in% names(ss), "Schema structure without nodes element")
    expect("edges" %in% names(ss), "Schema structure without edges element")
  }
})

context("Petstore sample API schemas structure graphs")
test_that("Create graphs of API schema structure", {
  petstore_spec <-
    system.file("extdata/sample_specs/petstore.json", package = "rapiclient")
  pet_api <- get_api(petstore_spec)
  schemas <- pet_api[["definitions"]]
  for(schema in schemas) {
    gr_dot <- get_schema_graphviz_dot(pet_api, schema)
    expect(
      grepl("digraph", gr_dot),
      "Can't find the 'digraph' keyword in dot string")
  }
})


