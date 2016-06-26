class ZCL_SWAG_EXAMPLE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SWAG_EXAMPLE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA(lo_swag) = NEW zcl_swag( ).

    DATA(lo_handler) = NEW zcl_swag_example_handler( ).
    lo_swag->register( lo_handler ).

    lo_swag->run( server ).

  ENDMETHOD.
ENDCLASS.