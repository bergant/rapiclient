
<img src ="https://travis-ci.org/bergant/rapiclient.svg?branch=master"/>

# __rapiclient__

<img align="right" src="img/rapiclient_ani.gif"/>



Access services specified in [OpenAPI](https://openapis.org) (formerly Swagger) format.

**rapiclient** is not a code generator. Client is generated dynamically as 
a list of R functions.


## Installation


```r
devtools::install_github("bergant/rapiclient")
```


## Usage

### Prepare API Operations and Schemas


```r
library(rapiclient)
```

This example uses the [sample petstore service](http://petstore.swagger.io)
and its OpenAPI definition at (http://petstore.swagger.io/v2/swagger.json).


```r
pet_api <- get_api(url = "http://petstore.swagger.io/v2/swagger.json")
operations <- get_operations(pet_api)
schemas <- get_schemas(pet_api)
```

Function `get_operations` returns a **list of functions**. 
Each function takes named arguments, converts the values to JSON 
according to API operation definition and performs a service call which
returns a http response object.

Function `get_schemas` returns a list of functions where each function returns 
an object according to the related schema in the API.


### Calling Service Operations

#### Find a Pet
Let's try to find a pet with Id = 42 (see operation [definition](http://petstore.swagger.io/#!/pet/getPetById)):

```r
res <- operations$getPetById(petId = 42)

res$status_code
# [1] 404
str(httr::content(res))
# List of 3
#  $ code   : int 1
#  $ type   : chr "error"
#  $ message: chr "Pet not found"
```

#### New Pet
OK, there is no pet with Id = 42, so let's [add a pet](http://petstore.swagger.io/#!/pet/addPet):


```r
res <- 
  operations$addPet(
    id = 42,
    category = schemas$Category(
      id = 1,
      name = "Undefined"
    ),
    name = "Agrajag",
    photoUrls = list(),
    tags = list(
      schemas$Tag(id = 1, name = "Wild"),
      schemas$Tag(id = 2, name = "Furry")
    ),
    status = "available"
  )

res$status_code
# [1] 200
```

Check:


```r
res <- operations$getPetById(petId = 42)

res$status_code
# [1] 200
str(httr::content(res))
# List of 6
#  $ id       : int 42
#  $ category :List of 2
#   ..$ id  : int 1
#   ..$ name: chr "Undefined"
#  $ name     : chr "Agrajag"
#  $ photoUrls: list()
#  $ tags     :List of 2
#   ..$ :List of 2
#   .. ..$ id  : int 1
#   .. ..$ name: chr "Wild"
#   ..$ :List of 2
#   .. ..$ id  : int 2
#   .. ..$ name: chr "Furry"
#  $ status   : chr "available"
```





### Help on API Operations

The good news is that autocomplete in RStudio editor works fine with dynamically created functions. The bad news: R documentation is not available 
with `help` or `?`. To lookup the operation definition
just print the function (write it down without parenthesis):

Let's get help for `getPetById`:

```r
operations$getPetById
# getPetById 
# Find pet by ID 
# Description:
#    Returns a single pet 
# 
# Parameters:
#   petId (integer)
#     ID of pet to return
```

More complicated `addPet` also describes the nested schemas:


```r
operations$addPet
# addPet 
# Add a new pet to the store 
# 
# Parameters:
#   id (integer)
#   category (Category)
#   name (string)
#   photoUrls (array[string])
#   tags (array[Tag])
#   status (string)
# Category 
#   id (integer)
#   name (string)
# Tag 
#   id (integer)
#   name (string)
```

For more detailed operation description use the operation's "definition" attribute :


```r
definition <- attr(operations$getPetById, "definition")
str(definition)
# List of 10
#  $ tags       : chr "pet"
#  $ summary    : chr "Find pet by ID"
#  $ description: chr "Returns a single pet"
#  $ operationId: chr "getPetById"
#  $ produces   : chr [1:2] "application/xml" "application/json"
#  $ parameters :'data.frame':	1 obs. of  6 variables:
#   ..$ name       : chr "petId"
#   ..$ in         : chr "path"
#   ..$ description: chr "ID of pet to return"
#   ..$ required   : logi TRUE
#   ..$ type       : chr "integer"
#   ..$ format     : chr "int64"
#  $ responses  :List of 3
#   ..$ 200:List of 2
#   .. ..$ description: chr "successful operation"
#   .. ..$ schema     :List of 1
#   .. .. ..$ $ref: chr "#/definitions/Pet"
#   ..$ 400:List of 1
#   .. ..$ description: chr "Invalid ID supplied"
#   ..$ 404:List of 1
#   .. ..$ description: chr "Pet not found"
#  $ security   :'data.frame':	1 obs. of  1 variable:
#   ..$ api_key:List of 1
#   .. ..$ : list()
#  $ path       : chr "/pet/{petId}"
#  $ action     : chr "get"
```


### Using Additional Headers

Set additional http headers at the time of creating operation functions
in `get_operations` function.

The following example uses New York Times API from [developer.nytimes.com](http://developer.nytimes.com/)
with API key authentication.



```r
nyt_api <- get_api("http://developer.nytimes.com/top_stories_v2.json/swagger.json")

nyt_operations <- 
  get_operations( nyt_api, .headers = c("api-key" = Sys.getenv("NYT_API_KEY")))

res <- nyt_operations$Top_Stories(section = "science", format = "json")

res$status_code
# [1] 200
 
content <- httr::content(res)
str(content, max.level = 1)
# List of 6
#  $ status      : chr "OK"
#  $ copyright   : chr "Copyright (c) 2017 The New York Times Company. All Rights Reserved."
#  $ section     : chr "science"
#  $ last_updated: chr "2017-01-30T08:47:55-05:00"
#  $ num_results : int 30
#  $ results     :List of 30
```



