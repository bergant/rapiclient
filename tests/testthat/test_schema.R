test_that("schema functions work", {
    pet_properties <- list(
        id = list(type = "integer", format = "int64"),
        name = list(type = "string", example = "doggie")
    )
    expect_identical(
        get_schema(
            list(
                definitions = list(
                    Pet = list(properties = pet_properties)
                )
            ),
            "Pet",
            compose_allOf = TRUE
        ),
        structure(
            list(properties = pet_properties),
            name = "Pet",
            class = c("rapi_schema", "list")
        )
    )
    expect_identical(
        get_schemas(
            list(
                definitions = list(
                    Pet = list(properties = list())
                )
            )
        ),
        structure(list(), .Names = character(0L))
    )
})
