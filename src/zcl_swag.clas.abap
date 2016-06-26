CLASS zcl_swag DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_url,
        regex       TYPE string,
        group_names TYPE STANDARD TABLE OF seosconame WITH DEFAULT KEY,
      END OF ty_url.
    TYPES:
      BEGIN OF ty_meta,
        summary     TYPE string,
        description TYPE string,
        url         TYPE ty_url,
        method      TYPE string,
        handler     TYPE string,
      END OF ty_meta.

    METHODS constructor
      IMPORTING
        !ii_server TYPE REF TO if_http_server.
    METHODS register
      IMPORTING
        !ii_handler TYPE REF TO zif_swag_handler.
    METHODS run.
    METHODS serve_spec.
  PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    ty_parameters_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_meta_internal,
      meta       TYPE ty_meta,
      obj        TYPE REF TO object,
      parameters TYPE ty_parameters_tt,
      classname  TYPE seoclsname,
    END OF ty_meta_internal .
  TYPES:
    ty_meta_internal_tt TYPE STANDARD TABLE OF ty_meta_internal WITH DEFAULT KEY .

  DATA mi_server TYPE REF TO if_http_server .
  DATA mt_meta TYPE ty_meta_internal_tt .
  CONSTANTS:
    BEGIN OF c_parm_kind,
      importing TYPE seopardecl VALUE '0',
      exporting TYPE seopardecl VALUE '1',	
      changing  TYPE seopardecl VALUE '2',	
      returning TYPE seopardecl VALUE '3',	
    END OF c_parm_kind .

  METHODS build_parameters
    IMPORTING
      !is_meta             TYPE ty_meta_internal
    RETURNING
      VALUE(rt_parameters) TYPE abap_parmbind_tab .
  METHODS create_data
    IMPORTING
      !is_meta       TYPE ty_meta_internal
    RETURNING
      VALUE(rr_data) TYPE REF TO data .
  METHODS from_input
    IMPORTING
      !is_meta TYPE ty_meta_internal
      !ir_ref  TYPE REF TO data .
  METHODS from_path
    IMPORTING
      !is_meta TYPE ty_meta_internal
      !ir_ref  TYPE REF TO data .
  METHODS json_reply
    IMPORTING
      !is_meta       TYPE ty_meta_internal
      !it_parameters TYPE abap_parmbind_tab
    RETURNING
      VALUE(rv_json) TYPE xstring .
  METHODS validate_parameters
    IMPORTING
      !it_parameters TYPE ty_parameters_tt .
ENDCLASS.



CLASS ZCL_SWAG IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lr_dref      TYPE REF TO data,
          lv_component TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    lr_dref = create_data( is_meta ).
    ASSIGN lr_dref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.
      ls_parameter-name  = <ls_parameter>-sconame.
      ls_parameter-value = REF #( <lg_comp> ).
      INSERT ls_parameter INTO TABLE rt_parameters.
    ENDLOOP.

    from_path( is_meta = is_meta
               ir_ref  = lr_dref ).

    from_input( is_meta = is_meta
                ir_ref  = lr_dref ).

  ENDMETHOD.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD create_data.

    DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
          lt_components TYPE cl_abap_structdescr=>component_table,
          lo_typedescr  TYPE REF TO cl_abap_typedescr,
          lv_name       TYPE string.


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


  METHOD from_input.

    DATA: lv_json TYPE xstring,
          lt_rtab TYPE abap_trans_resbind_tab,
          lv_bar  TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    DATA(lv_cdata) = mi_server->request->get_cdata( ).

    DATA(lo_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION demo_json_xml_to_upper
      SOURCE XML lv_cdata
      RESULT XML lo_writer.

    lv_json = lo_writer->get_output( ).

    LOOP AT is_meta-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>)
        WHERE pardecltyp = c_parm_kind-importing.
      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* ignore parameters that are part of url
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_rtab ASSIGNING FIELD-SYMBOL(<ls_rtab>).
      <ls_rtab>-name  = <ls_parameter>-sconame.
      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.
      <ls_rtab>-value = REF #( <lg_comp> ).
    ENDLOOP.

    IF lines( lt_rtab ) > 0.
      CALL TRANSFORMATION id
        SOURCE XML lv_json
        RESULT (lt_rtab).
    ENDIF.

  ENDMETHOD.


  METHOD from_path.

    DEFINE _store.
      READ TABLE is_meta-meta-url-group_names INDEX &1 INTO lv_component.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_component OF STRUCTURE <lg_struc> TO <lg_comp>.
        ASSERT sy-subrc = 0.
        <lg_comp> = lv_match&1.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lv_component TYPE string,
          lv_match1    TYPE string,
          lv_match2    TYPE string,
          lv_match3    TYPE string,
          lv_match4    TYPE string,
          lv_match5    TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    DATA(lv_path) = mi_server->request->get_header_field( '~path' ).
    FIND REGEX is_meta-meta-url-regex IN lv_path
      SUBMATCHES lv_match1 lv_match2 lv_match3 lv_match4 lv_match5.

    _store 1.
    _store 2.
    _store 3.
    _store 4.
    _store 5.

  ENDMETHOD.


  METHOD json_reply.

    DATA: lo_writer TYPE REF TO cl_sxml_string_writer.

    FIELD-SYMBOLS: <lg_struc> TYPE any.


    READ TABLE is_meta-parameters ASSIGNING FIELD-SYMBOL(<ls_meta>)
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>)
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      lo_writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).
      ASSIGN <ls_parameter>-value->* TO <lg_struc>.
      CALL TRANSFORMATION id
        SOURCE data = <lg_struc>
        RESULT XML lo_writer.
      rv_json = lo_writer->get_output( ).

    ENDIF.

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
      AND cmpname = ls_meta-meta-handler
      ORDER BY PRIMARY KEY.
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

        DATA(lv_json) = json_reply(
          is_meta       = <ls_meta>
          it_parameters = lt_parameters ).

        mi_server->response->set_data( lv_json ).
      ENDIF.
    ENDLOOP.

* todo, error if no handler found

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