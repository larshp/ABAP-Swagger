# ABAP-Swagger

An approach to expose ABAP REST services

## Usage

1: develop a class in ABAP with public methods

2: implement interface ZIF_SWAG_HANDLER, and register the public methods(example [method zif_swag_handler~meta](https://github.com/larshp/ABAP-Swagger/blob/master/src/example/zcl_swag_example_handler.clas.abap)

3: the methods are now accessible via rest([example](https://github.com/larshp/ABAP-Swagger/blob/master/src/example/zcl_swag_example.clas.abap))

4: the [Swagger UI](http://swagger.io/swagger-ui/) can be accessed via method GENERATE_UI

## Installing and Requirements

Install via [abapGit](http://www.abapgit.org)

Version requirement: see http://scn.sap.com/community/abap/blog/2013/01/07/abap-and-json or SAP note 1648418

