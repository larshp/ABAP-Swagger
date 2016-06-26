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

    DATA(lo_swag) = NEW zcl_swag( server ).

    DATA(lo_handler) = NEW zcl_swag_example_handler( ).
    lo_swag->register( lo_handler ).

    lo_swag->run( ).

  ENDMETHOD.
ENDCLASS.