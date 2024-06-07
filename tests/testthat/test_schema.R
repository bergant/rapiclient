test_that("schema functions work", {
    pet_properties <- list(
        id = list(type = "integer", format = "int64"),
        name = list(type = "string", example = "doggie")
    )
    schema <- get_schema(
        list(
            definitions = list(
                Pet = list(properties = pet_properties)
            )
        ),
        "Pet",
        compose_allOf = TRUE
    )
    expect_identical(
        schema,
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

    animal_properties <- list(
        name = list(type = "string", example = "dog")
    )
    schema <- get_schema(
        list(
            definitions = list(
                Pet = list(
                    allOf = list(
                        list(`$ref` = "#/definitions/Animal")
                    )
                ),
                Animal = list(properties =  animal_properties)
            )
        ),
        "Pet",
        compose_allOf = TRUE
    )
    expect_identical(
        schema$properties, animal_properties
    )
    expect_identical(
        schema$type, "object"
    )

    schema <- get_schema(
        list(
            definitions = list(
                Pet = list(
                    allOf = list(
                        list(`$ref` = "#/definitions/Animal")
                    ),
                    type = "household"
                ),
                Animal = list(properties =  animal_properties)
            )
        ),
        "Pet",
        compose_allOf = TRUE
    )
    expect_identical(
        schema$properties, animal_properties
    )
    expect_identical(
        schema$type, "household"
    )

    schema <- get_schema(
        list(
            definitions = list(
                House_Pet = list(properties = pet_properties)
            )
        ),
        "#/definitions/House Pet",
        compose_allOf = TRUE
    )
    expect_identical(
        schema,
        structure(
            list(properties = pet_properties),
            name = "House_Pet",
            class = c("rapi_schema", "list")
        )
    )

    api <- list(definitions = list(
        Pet = list(
            allOf = list(list( properties = animal_properties ))
        ),
        Animal = list(properties =  animal_properties)
    ))
    schema <- get_schema(api, "Pet", compose_allOf = TRUE)
    expect_identical(
        get_allOf(api, schema$allOf),
        animal_properties
    )

})
