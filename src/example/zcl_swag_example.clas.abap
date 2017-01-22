CLASS zcl_swag_example DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_EXAMPLE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: lo_swag    TYPE REF TO zcl_swag,
          lo_handler TYPE REF TO zcl_swag_example_handler.


    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = server
        iv_title  = 'Example'
        iv_base   = '/'.

    CREATE OBJECT lo_handler.
    lo_swag->register( lo_handler ).

    lo_swag->run( ).

  ENDMETHOD.
ENDCLASS.
