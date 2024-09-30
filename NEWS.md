# rapiclient 0.1.8

## User visible changes

* When present, combine operation parameters and api path-level parameters, 
where the former take precedence (@almahmoud, #23)

# rapiclient 0.1.7

## User visible changes

* `get_api` now has an `ext` argument for API urls that are missing a file
extension; this ensures that the file is properly parsed (@almahmoud, #26).

## Bug fixes and minor improvements

* `config` argument in `get_api` is passed along to the `GET` call for
non-public APIs that require a token (@almahmoud, #26).
* `get_api` handles both URLs and files (either `json` or `yml`)

# rapiclient 0.1.6

## User visible changes

* when functions that have signatures like `fun(x, ..., .__body__ = list(y))`,
where `x` is a argument for the 'URL' of the RESTful interface, and `y` is an
argument for the 'BODY' of POST and similar requests. The `...` provide backward
compatibility, and is used to populate elements of `.__body__`; the full
interface is required when URL and BODY have identically named arguments
(@mtmorgan)

## Bug fixes and minor improvements

* allow positional matching for `.body` arguments (@mtmorgan)
* when `.body` consists of 1 argument, it is represented as an unnamed set
(@mtmorgan)
* support argument names duplicated in `.body` (@mtmorgan)

# rapiclient 0.1.5

## New features

* Maintainer change.

# rapiclient 0.1.4

## Bug fixes and minor improvements

* Resolve check NOTES (@LiNk-NY, #24)
* Allow multiple `httr::accept` headers from `op_def$produces` (@almahmoud, #22)
* Work with `operation$parameters` that have zero length (@seandavi, #20)

