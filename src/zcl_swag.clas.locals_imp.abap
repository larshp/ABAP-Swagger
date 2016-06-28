*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_map_type DEFINITION.

  PUBLIC SECTION.
    METHODS:
      map
        IMPORTING is_parm        TYPE seosubcodf
        RETURNING VALUE(rv_type) TYPE string,
      map_internal
        IMPORTING io_typedescr   TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(rv_type) TYPE string,
      map_structure
        IMPORTING io_typedescr   TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(rv_type) TYPE string,
      map_table
        IMPORTING io_typedescr   TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(rv_type) TYPE string,
      map_element
        IMPORTING io_typedescr   TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(rv_type) TYPE string.

    CLASS-METHODS: get_typedescr
      IMPORTING is_parm             TYPE seosubcodf
      RETURNING VALUE(ro_typedescr) TYPE REF TO cl_abap_typedescr.

ENDCLASS.

CLASS lcl_map_type IMPLEMENTATION.

  METHOD map.
    rv_type = map_internal( get_typedescr( is_parm ) ).
  ENDMETHOD.

  METHOD map_internal.

    CASE io_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_type = map_element( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_struct.
        rv_type = map_structure( io_typedescr ).
      WHEN cl_abap_typedescr=>kind_table.
        rv_type = map_table( io_typedescr ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD map_element.

    CASE io_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_string
          OR cl_abap_typedescr=>typekind_char.
        rv_type = '"type":"string"'.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.

  METHOD map_structure.
* todo
    rv_type = '"schema":{"type":"object", "properties":{ "FOO":{ "type":"string" }, "BAR":{ "type":"string" } }}'.
  ENDMETHOD.

  METHOD map_table.
    BREAK-POINT.
  ENDMETHOD.

  METHOD get_typedescr.

    DATA: lv_name TYPE string.


    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = is_parm-type
      RECEIVING
        p_descr_ref    = ro_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
* try looking in the class
      ASSERT NOT is_parm-clsname IS INITIAL.
      CONCATENATE
        '\CLASS=' is_parm-clsname
        '\TYPE=' is_parm-type
        INTO lv_name.
      ro_typedescr = cl_abap_typedescr=>describe_by_name( lv_name ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.