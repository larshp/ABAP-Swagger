*----------------------------------------------------------------------*
*       CLASS ltcl_swag DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_swag DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PUBLIC SECTION.
    INTERFACES:
      if_http_server,
      if_http_request,
      if_http_response.

  PRIVATE SECTION.
    DATA: mo_swag          TYPE REF TO zcl_swag,
          mv_reply         TYPE string,
          mo_dummy_handler TYPE REF TO zcl_dummy_swagger_handler.

    METHODS: setup,
      given_a_foobar_meta
        IMPORTING
          iv_remove_data_object TYPE any OPTIONAL
            PREFERRED PARAMETER iv_remove_data_object,
      new_swag_instance,
      test FOR TESTING RAISING cx_static_check,
      remove_data_object_setting FOR TESTING.

    CLASS-METHODS: to_string
      IMPORTING iv_xstr       TYPE xstring
      RETURNING VALUE(rv_str) TYPE string.

ENDCLASS.       "ltcl_Register

*----------------------------------------------------------------------*
*       CLASS ltcl_swag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_swag IMPLEMENTATION.

  METHOD to_string.

    DATA: lo_conv TYPE REF TO cl_abap_conv_in_ce.


    lo_conv = cl_abap_conv_in_ce=>create( input = iv_xstr ).

    lo_conv->read( IMPORTING data = rv_str ).

  ENDMETHOD.                    "to_string

  METHOD setup.
    CREATE OBJECT mo_dummy_handler.

    me->if_http_server~request = me.
    me->if_http_server~response = me.

    new_swag_instance( ).
  ENDMETHOD.                    "setup

  METHOD if_http_request~get_method.
    method = zcl_swag=>c_method-get.
  ENDMETHOD.                    "if_http_request~get_method

  METHOD if_http_request~set_compression.
    RETURN.
  ENDMETHOD.


  METHOD if_http_entity~get_header_field.
    CASE name.
      WHEN '~path'.
        value = '/swag/foo/bar/'.
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( ).
    ENDCASE.
  ENDMETHOD.                    "if_http_entity~get_header_field

  METHOD if_http_entity~set_data.
    mv_reply = to_string( data ).
  ENDMETHOD.                    "if_http_entity~set_data


  METHOD if_http_entity~get_cdata.
    data = 'bar'.
  ENDMETHOD.                    "if_http_entity~get_cdata

  METHOD if_http_response~set_status.
    RETURN.
  ENDMETHOD.

  METHOD if_http_response~set_header_field.
    RETURN.
  ENDMETHOD.


  METHOD test.

    given_a_foobar_meta( ).

    mo_swag->register( mo_dummy_handler ).
    mo_swag->run( ).

    cl_abap_unit_assert=>assert_not_initial( mv_reply ).
    cl_abap_unit_assert=>assert_char_cp(
      act = mv_reply
      exp = '*foobar*' ).

  ENDMETHOD.                    "test

  METHOD if_http_entity~set_cdata.
    mv_reply = data.
  ENDMETHOD.                    "if_http_entity~set_data

  METHOD if_http_request~get_form_field.

    CASE name.
      WHEN 'IV_FOO'.
        value = 'foo'.
      WHEN 'IV_BAR'.
        value = 'bar'.

    ENDCASE.


  ENDMETHOD.

  METHOD remove_data_object_setting.

    "Keep DATA
    given_a_foobar_meta( iv_remove_data_object = abap_false ).

    mo_swag->register( mo_dummy_handler ).
    mo_swag->run( ).

    cl_abap_unit_assert=>assert_not_initial( mv_reply ).
    cl_abap_unit_assert=>assert_char_cp(
      act = mv_reply
      exp = '{"DATA":{"FOO":*' ).


    new_swag_instance( ).

    "Remove DATA
    given_a_foobar_meta( iv_remove_data_object = abap_true ).

    mo_swag->register( mo_dummy_handler ).
    mo_swag->run( ).

    cl_abap_unit_assert=>assert_not_initial( mv_reply ).
    cl_abap_unit_assert=>assert_char_cp(
      act = mv_reply
      exp = '{"FOO":*' ).

  ENDMETHOD.

  METHOD given_a_foobar_meta.

    DATA lt_foo_bar_meta TYPE zcl_swag=>ty_meta_tt.
    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF lt_foo_bar_meta.


    APPEND INITIAL LINE TO lt_foo_bar_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'this is the description'.
    <ls_meta>-url-regex = '/swag/(\w*)/(\w*)'.
    APPEND 'IV_FOO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BAR' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'THE_REAL_STUFF'.
    <ls_meta>-response_settings-remove_data_object = iv_remove_data_object.

    mo_dummy_handler->set_meta( lt_foo_bar_meta ).

  ENDMETHOD.

  METHOD new_swag_instance.

    CREATE OBJECT mo_swag
      EXPORTING
        ii_server = me
        iv_base   = ''
        iv_title  = 'test'.

  ENDMETHOD.

ENDCLASS.                    "ltcl_swag IMPLEMENTATION