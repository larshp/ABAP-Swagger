[![abaplint](https://abaplint.org/badges/larshp/ABAP-Swagger)](https://abaplint.org/project/larshp/ABAP-Swagger)

# ABAP-Swagger

An approach to expose ABAP REST services

## Usage

1: develop a class in ABAP with public methods

2: implement interface ZIF_SWAG_HANDLER, and register the public methods(example [method zif_swag_handler~meta](https://github.com/larshp/ABAP-Swagger/blob/master/src/example/zcl_swag_example_handler.clas.abap))

3: implement the SICF node handler (example [method if_http_extension~handle_request](https://github.com/larshp/ABAP-Swagger/blob/master/src/example/zcl_swag_example.clas.abap))

4: the methods are now accessible via rest

5: the [Swagger UI](http://swagger.io/swagger-ui/) can be accessed via `swagger.html` in the base dir of the REST services

## Installing and Requirements

Install via [abapGit](http://www.abapgit.org)

Requires [/UI2/CL_JSON](https://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer) for serializing/deserializing JSON. 

## JSON Request/Response
The JSON request body is given by the single importing parameter of your handler method.

```
SOME_STRUCTURE
  MY_PARAMETER_VALUE    type char10
  MY_HEADER             type HEADER_STRUCTURE
  SOME_ITEMS            type table ITEM_STRUCTURE

HEADER_STRUCTURE
  HEADER_VALUE_A  type char10
  OTHER_VALUE     type char10

ITEM
  ITEM_VALUE      type char10
  ITEM_VALUE_B    type char10
  ```
Expects the following JSON request body:
```
{
    "myParameterValue": "",
    "myHeader":{
        "headerValue": "",
        "otherValue": "",

    },
    "someItems":[
        {
            "itemValue"  : "",
            "itemValueB" : ""
        },
        {
            "itemValue"  : "",
            "itemValueB" : ""
        },
    ]
}
```
Likewise, the JSON response body is given by the returning parameter of your handler method.
