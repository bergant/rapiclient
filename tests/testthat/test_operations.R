test_that("API info, swagger, etc. elements are checked", {
    with_mocked_bindings(
        fetch_content = function(...) { "" },
        read_api_json = function(...) {
            list(swagger = NULL, info = TRUE, paths = "ok")
        },
        expect_warning(
            get_api(
                "https://www.example.com/swagger.json"
            ),
            "Missing Swagger Specification version"
        )
    )

    with_mocked_bindings(
        fetch_content = function(...) { "" },
        read_api_json = function(...) {
            list(info = NULL, swagger = TRUE, paths = "ok")
        },
        expect_warning(
            get_api(
                "https://www.example.com/swagger.json"
            ),
            "Missing Specification Info"
        )
    )

    with_mocked_bindings(
        fetch_content = function(...) { "" },
        read_api_json = function(...) {
            list(info = TRUE, swagger = TRUE, paths = NULL)
        },
        expect_warning(
            get_api(
                "https://www.example.com/swagger.json"
            ),
            "There is no paths element in the API specification"
        )
    )

    with_mocked_bindings(
        fetch_content = function(...) { "" },
        read_api_json = function(...) {
            list(info = TRUE, swagger = TRUE, paths = "ok")
        },
        expect_identical(
            get_api(
                "https://www.example.com:443/swagger.json"
            )$host,
            "www.example.com:443"
        )
    )

    with_mocked_bindings(
        fetch_content = function(...) { "" },
        read_api_json = function(...) {
            list(info = TRUE, swagger = TRUE, paths = "ok")
        },
        expect_error(
            get_api(
                "https://www.example.com:443/swagger.json",
                config = "bad"
            ),
            "'config' must be NULL or an instance of httr::config()"
        )
    )
})

test_that("get_message_body prints log", {
    old <- getOption("rapiclient.log_request")
    oldfile <- getOption("rapiclient.log_request_path")
    writefile <- "temp_rapiclient_log.json"
    options(
        rapiclient.log_request = TRUE,
        rapiclient.log_request_path = writefile
    )
    on.exit(options(rapiclient.log_request = old))
    on.exit(options(rapiclient.log_request_path = oldfile), add = TRUE)
    get_message_body(
        list(parameters = list(list(`in` = "body", name = "param1"))),
        list(param1 = "ok")
    )
    expect_true(file.exists(writefile))
    expect_identical(
        readLines(writefile),
        "\"ok\" "
    )
    file.remove(writefile)
})

test_that("*_for_status works", {
    response <- structure(
        list(
            url = "https://petstore.swagger.io/v2/pet/findByStatus",
            status_code = 400L,
            headers = structure(
                list(
                    `content-type` = "application/json"
                ), class = c("insensitive", "list")
            ),
            content = raw(0L)
        ), class = "response"
    )
    expect_error(
        content_or_stop(response),
        "Bad Request \\(HTTP 400\\)\\."
    )

    response <- structure(
        list(
            url = "https://petstore.swagger.io/v2/pet/findByStatus",
            status_code = 300L,
            headers = structure(
                list(
                    `content-type` = "application/json"
                ), class = c("insensitive", "list")
            ),
            content = raw(0L)
        ), class = "response"
    )
    expect_warning(
        content_or_warning(response),
        "Multiple Choices \\(HTTP 300\\)\\."
    )

    response <- structure(
        list(
            url = "https://petstore.swagger.io/v2/pet/findByStatus",
            status_code = 200L,
            headers = structure(
                list(
                    `content-type` = "application/json"
                ), class = c("insensitive", "list")
            ),
            content = raw(0L)
        ), class = "response"
    )
    expect_message(
        content_or_message(response),
        "OK \\(HTTP 200\\)\\."
    )

    dat <- list(id = 123L, name = "fluffy", status = "sold")
    response <- structure(
        list(
            url = "https://petstore.swagger.io/v2/pet/findByStatus",
            status_code = 200L,
            headers = structure(
                list(
                    `content-type` = "application/json"
                ), class = c("insensitive", "list")
            ),
            content = charToRaw(
                as.character(
                    jsonlite::toJSON(dat, auto_unbox = TRUE)
                )
            )
        ), class = "response"
    )
    expect_identical(
        content_or_stop(response),
        dat
    )
    expect_identical(
        content_or_warning(response),
        dat
    )
    ## message_for_status converts to NULL
    expect_identical(
        content_or_message(response),
        NULL
    )

    response <- 200L
    expect_identical(
        content_or_stop(response),
        200L
    )
    expect_identical(
        content_or_warning(response),
        200L
    )
    expect_message(
        content_or_message(response),
        "OK \\(HTTP 200\\)\\."
    )
})

