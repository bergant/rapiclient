.api_args <-
    function(formals, environment)
{
    arg_names <- if (is.null(names(formals))) character() else names(formals)
    arg_names <- arg_names[!arg_names %in% c("...", ".__body__")]
    args <- mget(arg_names, environment)
    args[!vapply(args, is.null, logical(1))]
}

.api_body <-
    function(formals, ..., .__body__)
{
    body0 <- formals$`.__body__`
    dot_args <- list(...)
    .__body__ <- .__body__[!vapply(.__body__, is.null, logical(1))]
    stopifnot(
        all(names(dot_args) %in% names(body0)),
        all(names(body) %in% names(body0)),
        `duplicate values for some '.__body__' arguments` =
            !any(names(dot_args) %in% names(.__body__))
    )
    body <- c(dot_args, .__body__)

    ## always named
    if (is.null(names(body)))
        names(body) <- rep("", length(body))

    ## positional matching of '...' and .__body__ args
    idx <- nzchar(names(body)) # named arguments
    available <- setdiff(names(body0), names(body)[idx])
    names(body)[!idx] <- available[seq_len(sum(!idx))]

    body
}

.get_url <-
    function(api, op_def, x)
{
    build_op_url(
        api, api$schemes[1], api$host, api$basePath, op_def, x
    )
}

.get_config <-
    function(api)
{
    api$config
}

.get_content_type <-
    function(op_def)
{
    type <- ifelse(
        is.null(op_def$consumes), "application/json", op_def$consumes
    )
    httr::content_type(type)
}

.get_accept <-
    function(op_def)
{
    ## claim that we will accept anything. OAS3.0 allows responses to
    ## support different content; our 2.x converter assumes the most
    ## specific (e.g., application/json), but then some responses
    ## (e.g., text/plain) generate errors.
    type <- "*/*"
    httr::accept(type)
}

.api_is_message_body_parameter <-
    function(op_def, x)
{
    parameters <- op_def$parameters
    vapply(parameters, function(parameter) {
        parameter[["in"]] %in% c("body", "formData")
    }, logical(1))
}
