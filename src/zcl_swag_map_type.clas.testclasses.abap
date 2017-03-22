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
    DATA: mo_map TYPE REF TO zcl_swag_map_type.

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
