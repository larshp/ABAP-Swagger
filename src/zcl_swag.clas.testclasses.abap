CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PUBLIC SECTION.
    INTERFACES: if_http_server, if_http_request.

  PRIVATE SECTION.
    DATA: mo_swag TYPE REF TO zcl_swag.

    METHODS: setup,
      test FOR TESTING.

ENDCLASS.       "ltcl_Register

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    me->if_http_server~request = me.

    CREATE OBJECT mo_swag
      EXPORTING
        ii_server = me.
  ENDMETHOD.

  METHOD if_http_entity~get_header_field.
    CASE name.
      WHEN '~path'.
        value = '/swag/foobar/'.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD test.

    DATA(lo_handler) = NEW zcl_swag_example_handler( ).
    mo_swag->register( lo_handler ).
    mo_swag->run( ).

  ENDMETHOD.

ENDCLASS.