CLASS zcl_swag DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_url,
        regex       TYPE string,
        group_names TYPE STANDARD TABLE OF seosconame WITH DEFAULT KEY,
      END OF ty_url .
    TYPES:
      BEGIN OF ty_meta,
        summary TYPE string,
        url     TYPE ty_url,
        method  TYPE string,
        handler TYPE string,
      END OF ty_meta .
    TYPES:
      ty_meta_tt TYPE STANDARD TABLE OF ty_meta WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF c_method,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        put    TYPE string VALUE 'PUT',
        delete TYPE string VALUE 'DELETE',
      END OF c_method .

    METHODS constructor
      IMPORTING
        !ii_server       TYPE REF TO if_http_server
        !iv_base         TYPE string
        !iv_swagger_json TYPE string DEFAULT '/swagger.json'
        !iv_swagger_html TYPE string DEFAULT '/swagger.html'
        !iv_title        TYPE string .
    METHODS register
      IMPORTING
        !ii_handler TYPE REF TO zif_swag_handler .
    METHODS run
      RAISING
        cx_static_check .
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

    DATA mv_base TYPE string .
    DATA mi_server TYPE REF TO if_http_server .
    DATA mt_meta TYPE ty_meta_internal_tt .
    CONSTANTS:
      BEGIN OF c_parm_kind,
        importing TYPE seopardecl VALUE '0',
        exporting TYPE seopardecl VALUE '1',
        changing  TYPE seopardecl VALUE '2',
        returning TYPE seopardecl VALUE '3',
      END OF c_parm_kind .
    DATA mv_swagger_json TYPE string .
    DATA mv_swagger_html TYPE string .
    DATA mv_title TYPE string .

    METHODS download
      IMPORTING
        !iv_file TYPE string .
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
    METHODS generate_spec
      IMPORTING
        !iv_title       TYPE clike
        !iv_description TYPE clike
      RETURNING
        VALUE(rv_spec)  TYPE string .
    METHODS generate_ui
      IMPORTING
        !iv_json_url TYPE string
        !iv_dist     TYPE string DEFAULT ''
        !iv_title    TYPE clike DEFAULT ''
      RETURNING
        VALUE(rv_ui) TYPE string .
    METHODS json_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS spec_parameters
      IMPORTING
        !is_meta       TYPE ty_meta_internal
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS spec_path
      IMPORTING
        !is_meta       TYPE ty_meta_internal
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS text_reply
      IMPORTING
        !is_meta       TYPE ty_meta_internal
        !it_parameters TYPE abap_parmbind_tab .
    METHODS validate_parameters
      IMPORTING
        !it_parameters TYPE ty_parameters_tt .
ENDCLASS.



CLASS ZCL_SWAG IMPLEMENTATION.


  METHOD build_parameters.

    DATA: ls_parameter LIKE LINE OF rt_parameters,
          lr_dref      TYPE REF TO data.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.


    lr_dref = create_data( is_meta ).
    ASSIGN lr_dref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>.
      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.
      ls_parameter-name  = <ls_parameter>-sconame.
      GET REFERENCE OF <lg_comp> INTO ls_parameter-value.
      INSERT ls_parameter INTO TABLE rt_parameters.
    ENDLOOP.

    from_path( is_meta = is_meta
               ir_ref  = lr_dref ).

    from_input( is_meta = is_meta
                ir_ref  = lr_dref ).

  ENDMETHOD.


  METHOD constructor.

    mi_server       = ii_server.
    mv_base         = iv_base.
    mv_swagger_json = iv_swagger_json.
    mv_swagger_html = iv_swagger_html.
    mv_title        = iv_title.

  ENDMETHOD.


  METHOD create_data.

    DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
          lt_components TYPE cl_abap_structdescr=>component_table,
          lo_typedescr  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components,
                   <ls_parameter> LIKE LINE OF is_meta-parameters.


    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
      <ls_component>-name = <ls_parameter>-sconame.

      lo_typedescr = lcl_map_type=>get_typedescr( <ls_parameter> ).

      <ls_component>-type ?= lo_typedescr.
    ENDLOOP.

    lo_struct = cl_abap_structdescr=>get( lt_components ).

    CREATE DATA rr_data TYPE HANDLE lo_struct.

  ENDMETHOD.


  METHOD download.
* this is a workaround to avoid CORS problems

* todo, possibility to define url
* todo, proxy settings

    DATA: li_client   TYPE REF TO if_http_client,
          lv_response TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'http://petstore.swagger.io/' && iv_file
        ssl_id             = 'ANONYM'
*        proxy_host         = lo_settings->get_proxy_url( )
*        proxy_service      = lo_settings->get_proxy_port( )
      IMPORTING
        client             = li_client ).

    li_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

    li_client->receive( ).

    lv_response = li_client->response->get_cdata( ).

    REPLACE FIRST OCCURRENCE
      OF '(t=t.requestInterceptor(t)||t),'
      IN lv_response
      WITH '(t=t.requestInterceptor(t)||t),t.credentials="same-origin",'.

    mi_server->response->set_cdata( lv_response ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD from_input.

    DATA: lv_cdata  TYPE string,
          lo_writer TYPE REF TO cl_sxml_string_writer,
          lv_json   TYPE xstring.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters,
                   <lg_comp>      TYPE any,
                   <lg_struc>     TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = c_parm_kind-importing.
      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* ignore parameters that are part of url
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_parameter>-sconame OF STRUCTURE <lg_struc> TO <lg_comp>.
      ASSERT sy-subrc = 0.

      IF <ls_parameter>-type = 'STRING'.
        <lg_comp> = mi_server->request->get_cdata( ).
      ELSEIF <ls_parameter>-type = 'XSTRING'.
        <lg_comp> = mi_server->request->get_data( ).
      ELSE.
        lv_cdata = mi_server->request->get_cdata( ).
        lv_cdata = '{"DATA":' && lv_cdata && '}'.

        lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

        CALL TRANSFORMATION zdemo_json_xml_to_upper
          SOURCE XML lv_cdata
          RESULT XML lo_writer.

        lv_json = lo_writer->get_output( ).

        CALL TRANSFORMATION id
          SOURCE XML lv_json
          RESULT data = <lg_comp>.
      ENDIF.

* multiple body input parameters not allowed
* todo, this should be validated earlier
      RETURN.

    ENDLOOP.

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

    DATA: lv_path      TYPE string,
          lv_component TYPE string,
          lv_match1    TYPE string,
          lv_match2    TYPE string,
          lv_match3    TYPE string,
          lv_match4    TYPE string,
          lv_match5    TYPE string.

    FIELD-SYMBOLS: <lg_comp>  TYPE any,
                   <lg_struc> TYPE any.


    ASSIGN ir_ref->* TO <lg_struc>.

    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).

    FIND REGEX is_meta-meta-url-regex IN lv_path
      SUBMATCHES lv_match1 lv_match2 lv_match3 lv_match4 lv_match5.

    _store 1.
    _store 2.
    _store 3.
    _store 4.
    _store 5.

  ENDMETHOD.


  METHOD generate_spec.

    DEFINE _add.
      CONCATENATE rv_spec &1 cl_abap_char_utilities=>newline
        INTO rv_spec ##NO_TEXT.
    END-OF-DEFINITION.

    TYPES: BEGIN OF ty_path,
             path TYPE string,
             meta LIKE mt_meta,
           END OF ty_path.

    DATA: lv_index     TYPE i,
          lt_paths     TYPE TABLE OF ty_path,
          lv_last_path TYPE abap_bool,
          lv_last_meta TYPE abap_bool,
          lv_path      TYPE string,
          lv_add       TYPE string.

    FIELD-SYMBOLS: <ls_path> LIKE LINE OF lt_paths,
                   <ls_meta> LIKE LINE OF mt_meta.


* handle path with multiple handlers(ie. GET and POST)
    LOOP AT mt_meta ASSIGNING <ls_meta>.
      lv_path = spec_path( <ls_meta> ).
      READ TABLE lt_paths ASSIGNING <ls_path> WITH KEY path = lv_path.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_paths ASSIGNING <ls_path>.
        <ls_path>-path = lv_path.
      ENDIF.
      APPEND <ls_meta> TO <ls_path>-meta.
    ENDLOOP.

    _add '{'.
    _add '  "swagger":"2.0",'.
    _add '  "info":{  '.
    _add '    "version":"1.0.0",'.

    lv_add = |    "title":"{ iv_title }",|.
    _add lv_add.
    lv_add = |    "description":"{ iv_description }"|.
    _add lv_add.

    _add '  },'.

    lv_add = |  "basePath":"{ mv_base }",|.
    _add lv_add.

    _add '  "schemes":['.
    _add '    "http"'.
    _add '  ],'.
    _add '  "consumes":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "produces":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "paths":{'.

    LOOP AT lt_paths ASSIGNING <ls_path>.
      lv_last_path = boolc( sy-tabix = lines( lt_paths ) ).

      lv_add = |    "{ <ls_path>-path }":\{|.
      _add lv_add.

      LOOP AT <ls_path>-meta ASSIGNING <ls_meta>.
        lv_last_meta = boolc( sy-tabix = lines( <ls_path>-meta ) ).

        lv_add = |      "{ to_lower( <ls_meta>-meta-method ) }":\{|.
        _add lv_add.
        lv_add = |        "summary":"{ <ls_meta>-meta-summary }",|.
        _add lv_add.
        lv_add = |        "description":"",|.
        _add lv_add.

        lv_add = spec_parameters( <ls_meta> ).
        _add lv_add.

        _add '        "produces":['.

        READ TABLE <ls_meta>-parameters WITH KEY
          pardecltyp = c_parm_kind-returning
          type = 'STRING' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          _add '"text/plain"'.
        ELSE.
          _add '"application/json"'.
        ENDIF.

        _add '        ],'.
        _add '        "responses":{'.
        _add '          "500":{'.
        _add '            "description":"error"'.
        _add '          }'.
        _add '        }'.
        _add '      }'.

        IF lv_last_meta = abap_false.
          _add ','.
        ENDIF.
      ENDLOOP.

      _add '    }'.
      IF lv_last_path = abap_false.
        _add ','.
      ENDIF.
    ENDLOOP.

    _add '  },'.
    _add '  "definitions":{'.
*    _add '    "Response":{'.
*    _add '      "type":"object",'.
*    _add '      "properties":{  '.
*    _add '        "FOO":{'.
*    _add '          "type":"string"'.
*    _add '        },'.
*    _add '        "BAR":{'.
*    _add '          "type":"string"'.
*    _add '        }'.
*    _add '      }'.
*    _add '    }'.
    _add '  }'.
    _add '}'.

    mi_server->response->set_cdata( rv_spec ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD generate_ui.
* todo, IV_DIST not supplyed from anywhere?

    DEFINE _add.
      CONCATENATE rv_ui &1 cl_abap_char_utilities=>newline
        INTO rv_ui ##NO_TEXT.
    END-OF-DEFINITION.

    _add '<!DOCTYPE html>'.
    _add '<html>'.
    _add '<head>'.
    _add '<meta charset="UTF-8">'.

    _add '<title>'.
    IF iv_title IS INITIAL.
      _add 'Swagger UI'.
    ELSE.
      _add iv_title.
    ENDIF.
    _add '</title>'.

* bad, temprary fix, should use read CDN instead of petstore.swagger.io
    _add '<link rel="icon" type="image/png" href="iv_dist/images/favicon-32x32.png" sizes="32x32" />'.
    _add '<link rel="icon" type="image/png" href="iv_dist/images/favicon-16x16.png" sizes="16x16" />'.
    _add '<link href="iv_dist/js/swagger-ui.css" media="screen" rel="stylesheet" type="text/css"/>'.
    _add '<script src="iv_base/js/swagger-ui-bundle.js" type="text/javascript"></script>'.
    _add '<script src="iv_base/js/swagger-ui-standalone-preset.js" type="text/javascript"></script>'.
    _add '</head>'.
    _add '<body>'.
    _add '<div id="swagger-ui"></div>'.
    _add '<script type="text/javascript">'.
    _add 'window.onload = function() {'.
    _add 'const ui = SwaggerUIBundle({'.
    _add 'url: "swagger.json",'.
    _add 'dom_id: "#swagger-ui",'.
    _add 'presets: ['.
    _add 'SwaggerUIBundle.presets.apis,'.
    _add 'Array.isArray(SwaggerUIStandalonePreset) ? SwaggerUIStandalonePreset : SwaggerUIStandalonePreset.default'.
    _add '],'.
    _add 'plugins: ['.
    _add 'SwaggerUIBundle.plugins.DownloadUrl'.
    _add '],'.
    _add 'layout: "StandaloneLayout"'.
    _add '})'.
    _add 'window.ui = ui'.
    _add '}'.
    _add '</script>'.
    _add '</body>'.
    _add '</html>'.

    IF iv_dist IS INITIAL.
      REPLACE ALL OCCURRENCES OF 'iv_dist'
        IN rv_ui WITH 'http://petstore.swagger.io' ##NO_TEXT.
    ELSE.
      REPLACE ALL OCCURRENCES OF 'iv_dist'
        IN rv_ui WITH iv_dist ##NO_TEXT.
    ENDIF.

    REPLACE ALL OCCURRENCES OF 'iv_base'
      IN rv_ui WITH mv_base ##NO_TEXT.

    REPLACE FIRST OCCURRENCE OF 'swagger.json'
      IN rv_ui WITH iv_json_url ##NO_TEXT.

    mi_server->response->set_cdata( rv_ui ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  ENDMETHOD.


  METHOD json_reply.

    DATA: lv_data   TYPE xstring,
          lo_writer TYPE REF TO cl_sxml_string_writer.

    FIELD-SYMBOLS: <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters,
                   <lg_struc>     TYPE any.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      lo_writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).
      ASSIGN <ls_parameter>-value->* TO <lg_struc>.
      CALL TRANSFORMATION id
        SOURCE data = <lg_struc>
        RESULT XML lo_writer.
      lv_data = lo_writer->get_output( ).

    ENDIF.

    mi_server->response->set_data( lv_data ).

  ENDMETHOD.


  METHOD register.

    DATA: ls_meta LIKE LINE OF mt_meta,
          lt_meta TYPE ty_meta_tt,
          lo_obj  TYPE REF TO cl_abap_objectdescr.


    lt_meta = ii_handler->meta( ).
    LOOP AT lt_meta INTO ls_meta-meta.
      ls_meta-obj = ii_handler.

      lo_obj ?= cl_abap_objectdescr=>describe_by_object_ref( ii_handler ).

      READ TABLE lo_obj->methods
        WITH KEY name = ls_meta-meta-handler
        visibility = cl_abap_objectdescr=>public
        TRANSPORTING NO FIELDS.
* method must exist and be public
      ASSERT sy-subrc = 0.

      ls_meta-classname = lo_obj->absolute_name+7.

      SELECT * FROM seosubcodf
        INTO TABLE ls_meta-parameters
        WHERE clsname = ls_meta-classname
        AND cmpname = ls_meta-meta-handler
        AND sconame NOT LIKE 'ZCX_%'
        ORDER BY PRIMARY KEY.                             "#EC CI_SUBRC
      ASSERT sy-subrc = 0.

      validate_parameters( ls_meta-parameters ).

      APPEND ls_meta TO mt_meta.

      CLEAR ls_meta.
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA: lv_path       TYPE string,
          lv_method     TYPE string,
          lt_parameters TYPE abap_parmbind_tab.

    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF mt_meta.


    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).
    lv_method = mi_server->request->get_method( ).

    IF lv_path = mv_base && mv_swagger_html.
      generate_ui(
        iv_json_url = mv_base && mv_swagger_json
        iv_title    = mv_title && ' - Swagger' ).
      RETURN.
    ELSEIF lv_path = mv_base && mv_swagger_json.
      generate_spec(
        iv_title       = mv_title
        iv_description = mv_title && ' REST functions' ).
      RETURN.
    ELSEIF lv_path = mv_base && '/js/swagger-ui-bundle.js'.
      download( '/js/swagger-ui-bundle.js' ).
      RETURN.
    ELSEIF lv_path = mv_base && '/js/swagger-ui-standalone-preset.js'.
      download( '/js/swagger-ui-standalone-preset.js' ).
      RETURN.
    ENDIF.

    LOOP AT mt_meta ASSIGNING <ls_meta>.
      IF lv_method <> <ls_meta>-meta-method.
        CONTINUE.
      ENDIF.

      FIND REGEX <ls_meta>-meta-url-regex IN lv_path.
      IF sy-subrc = 0.

        lt_parameters = build_parameters( <ls_meta> ).
        CALL METHOD <ls_meta>-obj->(<ls_meta>-meta-handler)
          PARAMETER-TABLE lt_parameters.

        mi_server->response->set_compression( ).

        LOOP AT <ls_meta>-parameters
            TRANSPORTING NO FIELDS
            WHERE pardecltyp = c_parm_kind-returning
            AND ( type = 'STRING' OR type = 'XSTRING' ).
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          text_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        ELSE.
          json_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        ENDIF.

        mi_server->response->set_status( code = 200 reason = '200' ).
        RETURN.

      ENDIF.
    ENDLOOP.

    mi_server->response->set_cdata( '404 swagger' ).
    mi_server->response->set_status( code = 404 reason = '404' ).

  ENDMETHOD.


  METHOD spec_parameters.

    DATA: lt_string TYPE TABLE OF string,
          ls_string LIKE LINE OF lt_string,
          lv_type   TYPE string,
          lo_map    TYPE REF TO lcl_map_type.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters.


    APPEND '"parameters":[' TO lt_string.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = c_parm_kind-importing.

      APPEND '{' TO lt_string.

      CONCATENATE '"name":"' <ls_parameter>-sconame '",' INTO ls_string.
      APPEND ls_string TO lt_string.
*      APPEND |"name":"{ <ls_parameter>-sconame }",|  TO lt_string.

      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND '"in":"path",' TO lt_string.
      ELSEIF is_meta-meta-method = c_method-get.
        APPEND '"in":"query",' TO lt_string.
      ELSE.
        APPEND '"in":"body",' TO lt_string.
      ENDIF.

      APPEND '"description":"",' TO lt_string.

      CREATE OBJECT lo_map.
      lv_type = lo_map->map( <ls_parameter> ).
      CONCATENATE lv_type ',' INTO ls_string.
      APPEND ls_string TO lt_string.
*      APPEND |{ lv_type },| TO lt_string.
*      APPEND |"type":"string",|  TO lt_string.
*      APPEND |"schema":\{{ lv_type }\},| TO lt_string.

      APPEND '"required":true'   TO lt_string.
      APPEND '},' TO lt_string.
    ENDLOOP.
    IF sy-subrc = 0.
* fix the comma
      DELETE lt_string INDEX lines( lt_string ).
      APPEND '}' TO lt_string.
    ENDIF.

    APPEND '],' TO lt_string.

    CONCATENATE LINES OF lt_string INTO rv_path
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD spec_path.

    DATA: lv_name    TYPE string,
          lv_offset1 TYPE i,
          lv_offset2 TYPE i.


    rv_path = is_meta-meta-url-regex.

    REPLACE ALL OCCURRENCES OF '$' IN rv_path WITH ''.

* replace the regex groups like (\w*) with swagger identifies {IV_NAME}
    LOOP AT is_meta-meta-url-group_names INTO lv_name.
      FIND FIRST OCCURRENCE OF '(' IN rv_path MATCH OFFSET lv_offset1.
      FIND FIRST OCCURRENCE OF ')' IN rv_path MATCH OFFSET lv_offset2.
      lv_offset2 = lv_offset2 + 1.
      CONCATENATE rv_path(lv_offset1) '{' lv_name '}' rv_path+lv_offset2 INTO rv_path.
    ENDLOOP.

  ENDMETHOD.


  METHOD text_reply.

    FIELD-SYMBOLS: <lg_any>       TYPE any,
                   <ls_meta>      LIKE LINE OF is_meta-parameters,
                   <ls_parameter> LIKE LINE OF it_parameters.


    READ TABLE is_meta-parameters ASSIGNING <ls_meta>
      WITH KEY pardecltyp = c_parm_kind-returning.
    IF sy-subrc  = 0.
      READ TABLE it_parameters ASSIGNING <ls_parameter>
        WITH KEY name = <ls_meta>-sconame.
      ASSERT sy-subrc = 0.

      ASSIGN <ls_parameter>-value->* TO <lg_any>.

      CASE <ls_meta>-type.
        WHEN 'XSTRING'.
          mi_server->response->set_data( <lg_any> ).
        WHEN 'STRING'.
          mi_server->response->set_cdata( <lg_any> ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.

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

* todo, max one importing parameter? apart from path parameters?

  ENDMETHOD.
ENDCLASS.
