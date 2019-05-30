*----------------------------------------------------------------------*
*       CLASS ltcl_map_type DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_map_type DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_map TYPE REF TO zcl_swag_map_type.

    METHODS:
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

  METHOD string.

    DATA: lv_type TYPE string,
          ls_param TYPE seosubcodf.


    ls_param-type = 'STRING'.

    CREATE OBJECT mo_map
      EXPORTING
        is_param = ls_param.

    lv_type = mo_map->map( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = '"type":"string"' ).

  ENDMETHOD.                    "string

  METHOD char.

    DATA: lv_type TYPE string,
          ls_param TYPE seosubcodf.


    ls_param-type = 'XUBNAME'.

    CREATE OBJECT mo_map
      EXPORTING
        is_param = ls_param.

    lv_type = mo_map->map( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = '"type":"string"' ).

  ENDMETHOD.                    "char

  METHOD structure.

    DATA: lv_type TYPE string,
          ls_param TYPE seosubcodf.


    ls_param-type = 'USR02'.

    CREATE OBJECT mo_map
      EXPORTING
        is_param = ls_param.

    lv_type = mo_map->map( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_type
      exp = '*"type":"object"*' ).

  ENDMETHOD.                    "structure

  METHOD table.

    DATA: lv_type TYPE string,
          ls_param TYPE seosubcodf.


    ls_param-type = 'STRING_TABLE'.

    CREATE OBJECT mo_map
      EXPORTING
        is_param = ls_param.

    lv_type = mo_map->map( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_type
      exp = '*"type":"array"*' ).

  ENDMETHOD.

ENDCLASS.                    "ltcl_map_type IMPLEMENTATION
