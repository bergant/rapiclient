context("POST get_message_body")

test_that('only "in": "body" parameter is found', {
    op_def <- list(parameters = list( list(`in` = "body", name = "param1") ))

    expect <- structure("{}", class = "json")
    x <- setNames(list(), character())
    expect_identical(expect , get_message_body(op_def, x))
    x <- list(param0 = "bad")
    expect_identical(expect, get_message_body(op_def, x))

    expect <- structure('"ok"', class = "json")
    x <- list(param1 = "ok")
    expect_identical(expect, get_message_body(op_def, x))
    x <- list(param0 = "bad", param1 = "ok")
    expect_identical(expect, get_message_body(op_def, x))
})

test_that("unboxing works", {
    op_def <- list(parameters = list( list(`in` = "body", name = "param1") ))

    expect <- structure('"ok"', class = "json")
    x <- list(param1 = "ok")
    expect_identical(expect, get_message_body(op_def, x))

    expect <- structure('["ok"]', class = "json")
    x <- list(param1 = I("ok"))
    expect_identical(expect, get_message_body(op_def, x))

    expect <- structure('["ok", "ok"]', class = "json")
    x <- list(param1 = c("ok", "ok"))
    expect_identical(expect, get_message_body(op_def, x))
    x <- list(param1 = I(c("ok", "ok")))
    expect_identical(expect, get_message_body(op_def, x))

    expect <- structure('[\n  "ok"\n]', class = "json")
    x <- list(param1 = list("ok"))
    expect_identical(expect, get_message_body(op_def, x))

    expect <- structure('[\n  "ok",\n  "ok"\n]', class = "json")
    x <- list(param1 = list("ok", "ok"))
    expect_identical(expect, get_message_body(op_def, x))
})
