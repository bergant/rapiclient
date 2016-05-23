# rapiclient



**(Experimental)** Get an R API client from Swagger representation as
dynamically created functions.

## Install


```r
devtools::install_github("bergant/rapiclient")
```


## Usage

### Prepare API Operations and Schemas


```r
library(rapiclient)
```

Get API definition from [sample petstore service](http://petstore.swagger.io):


```r
pet_api <- get_api(url = "http://petstore.swagger.io/v2/swagger.json")
operations <- get_operations(pet_api)
schemas <- get_schemas(pet_api)
```

Function `get_operations` returns a **list of functions** according to API operations definition.

And `get_schemas` returns a list of functions each returning an object 
according to the related schema in the API.


## Calling Service Operations

### Find a Pet
Let's try to find a pet with Id = 42 (see operation definition):

```r
res <- operations$getPetById(petId = 42)

res$status_code
```

```
## [1] 404
```

```r
str(httr::content(res))
```

```
## List of 3
##  $ code   : int 1
##  $ type   : chr "error"
##  $ message: chr "Pet not found"
```

### New Pet
OK, there is no pet with Id = 42, so let's add a new pet:


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
```

```
## [1] 200
```

Check:


```r
res <- operations$getPetById(petId = 42)

res$status_code
```

```
## [1] 200
```

```r
str(httr::content(res))
```

```
## List of 6
##  $ id       : int 42
##  $ category :List of 2
##   ..$ id  : int 1
##   ..$ name: chr "Undefined"
##  $ name     : chr "Agrajag"
##  $ photoUrls: list()
##  $ tags     :List of 2
##   ..$ :List of 2
##   .. ..$ id  : int 1
##   .. ..$ name: chr "Wild"
##   ..$ :List of 2
##   .. ..$ id  : int 2
##   .. ..$ name: chr "Furry"
##  $ status   : chr "available"
```





## Help on API Operations

In RStudio you can use autocomplete with dynamically created functions
but you can't get an R documentation
with `help` or `?`. To lookup the operation definition
just print the function (write it down without parenthesis):

Let's get help for `getPetById`:

```r
operations$getPetById
```

```
## getPetById 
## Find pet by ID 
## Description:
##    Returns a single pet 
## 
## Parameters:
##   petId (integer)
##     ID of pet to return
```

More complicated `addPet` also describes the nested schemas:


```r
operations$addPet
```

```
## addPet 
## Add a new pet to the store 
## 
## Parameters:
##   id (integer)
##   category (Category)
##   name (string)
##   photoUrls (array[string])
##   tags (array[Tag])
##   status (string)
## Category 
##   id (integer)
##   name (string)
## Tag 
##   id (integer)
##   name (string)
```

