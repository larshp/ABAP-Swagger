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

  methods SERVE_SPEC .
  methods CONSTRUCTOR
    importing
      !II_SERVER type ref to IF_HTTP_SERVER .
  methods REGISTER
    importing
      !II_HANDLER type ref to ZIF_SWAG_HANDLER .
  methods RUN .
protected section.
private section.

  types:
    BEGIN OF ty_meta_internal,
      meta   TYPE ty_meta,
      obj    TYPE REF TO object,
      method TYPE abap_methdescr,
    END OF ty_meta_internal .
  types:
    ty_meta_internal_tt TYPE STANDARD TABLE OF ty_meta_internal WITH DEFAULT KEY .

  data MI_SERVER type ref to IF_HTTP_SERVER .
  data MT_META type TY_META_INTERNAL_TT .

  methods BUILD_PARAMETERS
    returning
      value(RT_PARAMETERS) type ABAP_PARMBIND_TAB .
ENDCLASS.



CLASS ZCL_SWAG IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lo_struct    TYPE REF TO cl_abap_structdescr,
          lr_dref      TYPE REF TO data.

    FIELD-SYMBOLS: <comp>  TYPE any,
                   <struc> TYPE any.


    lo_struct = cl_abap_structdescr=>get(
      VALUE #(
        ( name = 'IV_FOO' type = cl_abap_elemdescr=>get_string( ) )
        ( name = 'RS_DATA' type = cl_abap_elemdescr=>get_string( ) )
             ) ).

    CREATE DATA lr_dref TYPE HANDLE lo_struct.

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

  ENDMETHOD.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD register.

    DATA: ls_meta LIKE LINE OF mt_meta.


    ls_meta-obj = ii_handler.

    ls_meta-meta = ii_handler->meta( ).

    DATA(lo_obj) = CAST cl_abap_objectdescr(
      cl_abap_objectdescr=>describe_by_object_ref( ii_handler ) ).

    READ TABLE lo_obj->methods INTO ls_meta-method
      WITH KEY name = ls_meta-meta-handler.
    ASSERT sy-subrc = 0.

    APPEND ls_meta TO mt_meta.

  ENDMETHOD.


  METHOD run.

    DATA: lt_parameters TYPE abap_parmbind_tab.


    DATA(lv_path) = mi_server->request->get_header_field( '~path' ).

    LOOP AT mt_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      FIND REGEX <ls_meta>-meta-url-regex IN lv_path.
      IF sy-subrc = 0.

        lt_parameters = build_parameters( ).
        CALL METHOD <ls_meta>-obj->(<ls_meta>-meta-handler)
          PARAMETER-TABLE lt_parameters.
* todo

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method SERVE_SPEC.
  endmethod.
ENDCLASS.