The Swagger Petstore is an example API used for testing. 

The files were obtained from swagger.io.

In JSON format:

```r
download.file(
  url = "http://petstore.swagger.io/v2/swagger.json",
  destfile = "inst/extdata/sample_specs/petstore.json"
)
```

In YAML format: 

```r
download.file(
  url = "http://petstore.swagger.io/v2/swagger.yaml",
  destfile = "inst/extdata/sample_specs/petstore.yaml"
)
```

