CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PUBLIC SECTION.
    INTERFACES:
      if_http_server PARTIALLY IMPLEMENTED,
      if_http_request PARTIALLY IMPLEMENTED,
      if_http_response PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.
    DATA: mo_swag  TYPE REF TO zcl_swag,
          mv_reply TYPE string.

    METHODS: setup,
      test FOR TESTING.

    CLASS-METHODS: to_string
      IMPORTING
                iv_xstr       TYPE xstring
      RETURNING VALUE(rv_str) TYPE string.

ENDCLASS.       "ltcl_Register

CLASS ltcl_test IMPLEMENTATION.

  METHOD to_string.

    DATA(lo_conv) = cl_abap_conv_in_ce=>create( input = iv_xstr ).

    lo_conv->read(
      IMPORTING
        data = rv_str ).

  ENDMETHOD.

  METHOD setup.
    me->if_http_server~request = me.
    me->if_http_server~response = me.

    CREATE OBJECT mo_swag
      EXPORTING
        ii_server = me.
  ENDMETHOD.

  METHOD if_http_entity~get_header_field.
    CASE name.
      WHEN '~path'.
        value = '/swag/foo/'.
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( ).
    ENDCASE.
  ENDMETHOD.

  METHOD if_http_entity~set_data.
    mv_reply = to_string( data ).
  ENDMETHOD.

  METHOD if_http_entity~get_cdata.
    data = '{"IV_BAR":"bar"}'.
  ENDMETHOD.

  METHOD test.

    DATA(lo_handler) = NEW zcl_swag_example_handler( ).
    mo_swag->register( lo_handler ).
    mo_swag->run( ).

    cl_abap_unit_assert=>assert_not_initial( mv_reply ).
    cl_abap_unit_assert=>assert_char_cp(
      act = mv_reply
      exp = '*foobar*' ).

  ENDMETHOD.

ENDCLASS.