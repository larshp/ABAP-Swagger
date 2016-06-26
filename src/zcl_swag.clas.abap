class ZCL_SWAG definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_url,
             regex       TYPE string,
             group_names TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           END OF ty_url .
  types:
    BEGIN OF ty_meta,
             summary     TYPE string,
             description TYPE string,
             url         TYPE ty_url,
             method      TYPE string,
             handler     TYPE string,
           END OF ty_meta .

  methods CONSTRUCTOR
    importing
      !II_SERVER type ref to IF_HTTP_SERVER .
  methods REGISTER
    importing
      !II_HANDLER type ref to ZIF_SWAG_HANDLER .
  methods RUN .
  methods SERVE_SPEC .
protected section.
private section.

  types:
    ty_parameters_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
  types:
    BEGIN OF ty_meta_internal,
      meta       TYPE ty_meta,
      obj        TYPE REF TO object,
      parameters TYPE ty_parameters_tt,
      classname  TYPE seoclsname,
    END OF ty_meta_internal .
  types:
    ty_meta_internal_tt TYPE STANDARD TABLE OF ty_meta_internal WITH DEFAULT KEY .

  data MI_SERVER type ref to IF_HTTP_SERVER .
  data MT_META type TY_META_INTERNAL_TT .
  constants:
    BEGIN OF c_parm_kind,
               importing TYPE seopardecl VALUE '0',
               exporting TYPE seopardecl VALUE '1',	
               changing  TYPE  seopardecl VALUE '2',	
               returning TYPE seopardecl VALUE '3',	
             END OF c_parm_kind .

  methods BUILD_PARAMETERS
    importing
      !IS_META type TY_META_INTERNAL
    returning
      value(RT_PARAMETERS) type ABAP_PARMBIND_TAB .
  methods CREATE_DATA
    importing
      !IS_META type TY_META_INTERNAL
    returning
      value(RR_DATA) type ref to DATA .
  methods VALIDATE_PARAMETERS
    importing
      !IT_PARAMETERS type TY_PARAMETERS_TT .
ENDCLASS.



CLASS ZCL_SWAG IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lr_dref      TYPE REF TO data.

    FIELD-SYMBOLS: <comp>  TYPE any,
                   <struc> TYPE any.


    lr_dref = create_data( is_meta ).
    ASSIGN lr_dref->* TO <struc>.

    ASSIGN COMPONENT 'IV_FOO' OF STRUCTURE <struc> TO <comp>.
    <comp> = 'test'.
    ls_parameter-name  = 'IV_FOO'.
    ls_parameter-value = REF #( <comp> ).
    INSERT ls_parameter INTO TABLE rt_parameters.

    ASSIGN COMPONENT 'RS_DATA' OF STRUCTURE <struc> TO <comp>.
    ls_parameter-name  = 'RS_DATA'.
    ls_parameter-value = REF #( <comp> ).
    INSERT ls_parameter INTO TABLE rt_parameters.

*    ASSIGN COMPONENT 'IV_FOO' OF STRUCTURE <struc> TO <comp>.
*    <comp> = 'test'.

  ENDMETHOD.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD create_data.

    DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
          lt_components TYPE cl_abap_structdescr=>component_table,
          lo_typedescr  TYPE REF TO cl_abap_typedescr,
          lv_name       TYPE string.

    FIELD-SYMBOLS: <comp>  TYPE any,
                   <struc> TYPE any.


    LOOP AT is_meta-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      APPEND INITIAL LINE TO lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
      <ls_component>-name = <ls_parameter>-sconame.

      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = <ls_parameter>-type
        RECEIVING
          p_descr_ref    = lo_typedescr
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
* try looking in the class
        CONCATENATE '\CLASS=' is_meta-classname '\TYPE=' <ls_parameter>-type INTO lv_name.
        lo_typedescr = cl_abap_typedescr=>describe_by_name( lv_name ).
      ENDIF.

      <ls_component>-type ?= lo_typedescr.
    ENDLOOP.

    lo_struct = cl_abap_structdescr=>get( lt_components ).

    CREATE DATA rr_data TYPE HANDLE lo_struct.

  ENDMETHOD.


  METHOD register.

    DATA: ls_meta LIKE LINE OF mt_meta.


    ls_meta-obj = ii_handler.

    ls_meta-meta = ii_handler->meta( ).

    DATA(lo_obj) = CAST cl_abap_objectdescr(
      cl_abap_objectdescr=>describe_by_object_ref( ii_handler ) ).

    ls_meta-classname = lo_obj->absolute_name+7.

    SELECT * FROM seosubcodf
      INTO TABLE ls_meta-parameters
      WHERE clsname = ls_meta-classname
      AND cmpname = ls_meta-meta-handler.
    ASSERT sy-subrc = 0.

    validate_parameters( ls_meta-parameters ).

    APPEND ls_meta TO mt_meta.

  ENDMETHOD.


  METHOD run.

    DATA: lt_parameters TYPE abap_parmbind_tab.


    DATA(lv_path) = mi_server->request->get_header_field( '~path' ).

    LOOP AT mt_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      FIND REGEX <ls_meta>-meta-url-regex IN lv_path.
      IF sy-subrc = 0.

        lt_parameters = build_parameters( <ls_meta> ).
        CALL METHOD <ls_meta>-obj->(<ls_meta>-meta-handler)
          PARAMETER-TABLE lt_parameters.
* todo

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serve_spec.

* todo
    BREAK-POINT.

  ENDMETHOD.


  METHOD validate_parameters.

* no EXPORTING, no CHANGING
    LOOP AT it_parameters TRANSPORTING NO FIELDS
        WHERE pardecltyp = c_parm_kind-exporting
        OR pardecltyp = c_parm_kind-changing.
      ASSERT 0 = 1.
    ENDLOOP.

* no reference types
* todo

  ENDMETHOD.
ENDCLASS.