test_that("API info, swagger, etc. elements are checked", {
    with_mocked_bindings(
        get_api_json = function(...) {
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
        get_api_json = function(...) {
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
        get_api_json = function(...) {
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
        get_api_json = function(...) {
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
        get_api_json = function(...) {
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
