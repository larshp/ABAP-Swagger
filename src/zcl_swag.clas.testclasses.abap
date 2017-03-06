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
    DATA: mo_swag  TYPE REF TO zcl_swag,
          mv_reply TYPE string.

    METHODS: setup,
      test FOR TESTING.

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

    lo_conv->read(
      IMPORTING
        data = rv_str ).

  ENDMETHOD.                    "to_string

  METHOD setup.
    me->if_http_server~request = me.
    me->if_http_server~response = me.

    CREATE OBJECT mo_swag
      EXPORTING
        ii_server = me
        iv_base   = ''
        iv_title  = 'test'.
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
        value = '/swag/foo/'.
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

  METHOD test.

    DATA: lo_handler TYPE REF TO zcl_swag_example_handler.


    CREATE OBJECT lo_handler.
    mo_swag->register( lo_handler ).
    mo_swag->run( ).

    cl_abap_unit_assert=>assert_not_initial( mv_reply ).
    cl_abap_unit_assert=>assert_char_cp(
      act = mv_reply
      exp = '*foobar*' ).

  ENDMETHOD.                    "test

ENDCLASS.                    "ltcl_swag IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_map_type DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_map_type DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PUBLIC SECTION.
    INTERFACES:
      if_http_server.

  PRIVATE SECTION.
    DATA: mo_map TYPE REF TO lcl_map_type.

    METHODS:
      setup,
      string FOR TESTING,
      char FOR TESTING,
      table FOR TESTING,
      structure FOR TESTING.

ENDCLASS.                    "ltcl_map_type DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_map_type IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_map_type IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_map.
  ENDMETHOD.                    "setup

  METHOD string.

    DATA: lv_type TYPE string,
          ls_parm TYPE seosubcodf.


    ls_parm-type = 'STRING'.
    lv_type = mo_map->map( ls_parm ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = '"type":"string"' ).

  ENDMETHOD.                    "string

  METHOD char.

    DATA: lv_type TYPE string,
          ls_parm TYPE seosubcodf.


    ls_parm-type = 'ZAGS_REPO_NAME'.

    lv_type = mo_map->map( ls_parm ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = '"type":"string"' ).

  ENDMETHOD.                    "char

  METHOD structure.

    DATA: lv_type TYPE string,
          ls_parm TYPE seosubcodf.


    ls_parm-type = 'USR02'.
    lv_type = mo_map->map( ls_parm ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_type
      exp = '*"type":"object"*' ).

  ENDMETHOD.                    "structure

  METHOD table.

    DATA: lv_type TYPE string,
          ls_parm TYPE seosubcodf.


    ls_parm-type = 'STRING_TABLE'.
    lv_type = mo_map->map( ls_parm ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_type
      exp = '*"type":"array"*' ).

  ENDMETHOD.

ENDCLASS.                    "ltcl_map_type IMPLEMENTATION
