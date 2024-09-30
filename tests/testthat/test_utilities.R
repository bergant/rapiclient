test_that(".api_args works with inputs", {
    expect_error(
        .api_args(pairlist(a = 1, b = 2), list2env(list(a = 100))),
    )
    expect_identical(
        .api_args(pairlist(a = 1, b = 2), list2env(list(a = 100, b = 200))),
        list(a = 100, b = 200)
    )
    expect_identical(
        .api_args(
            pairlist(a = 1, b = 2), list2env(list(a = 100, b = 200, c = 300))
        ),
        list(a = 100, b = 200)
    )
})

test_that(".api_body works with inputs", {
    expect_identical(
        .api_body(
            pairlist(a = 1, .__body__ = list(a = 1)),
            .__body__= list(a = 100, b = NULL)
        ),
        list(a = 100)
    )
    expect_error(
        .api_body(
            pairlist(a = 1, .__body__ = list(a = 1)),
            b = 100,
            .__body__= list(a = 100, b = NULL)
        )
    )
    expect_error(
        .api_body(
            pairlist(a = 1, .__body__ = list(a = 1)),
            .__body__= list(c = 3)
        )
    )
    expect_error(
        .api_body(
            pairlist(a = 1, .__body__ = list(a = 1)),
            a = 100,
            .__body__= list(a = 100)
        ),
        "duplicate values.*"
    )
    expect_identical(
        .api_body(
            pairlist(a = 1, .__body__ = list(a = NULL)),
            .__body__= list(a = NULL)
        ),
        structure(list(), .Names = character(0L))
    )
})

test_that(".get_content_type works", {
    expect_identical(
        .get_content_type(list(consumes = "application/json")),
        httr::content_type("application/json")
    )
    expect_identical(
        .get_content_type(list(consumes = NULL)),
        httr::content_type("application/json")
    )
})

test_that(".get_accept works", {
    expect_identical(
        .get_accept(list(produces = "application/json")),
        httr::accept("*/*")
    )
    expect_identical(
        .get_accept(list(produces = "*/*")),
        httr::accept("*/*")
    )
    expect_identical(
        .get_accept(list(produces = NULL)),
        httr::accept("*/*")
    )
})

test_that(".api_is_message_body_parameter works", {
    expect_true(
        .api_is_message_body_parameter(
            list(parameters = list(list(`in` = "body")))
        )
    )
    expect_true(
        .api_is_message_body_parameter(
            list(parameters = list(list(`in` = "formData")))
        )
    )
    expect_false(
        .api_is_message_body_parameter(
            list(parameters = list(list(`in` = "not")))
        )
    )
})

test_that("other helpers work", {
    expect_identical(
        .get_config(list(config = list(a = 1, b = 2))),
        list(a = 1, b = 2)
    )
})
