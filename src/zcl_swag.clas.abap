class zcl_swag definition
  public
  create public .

  public section.

    constants:
      begin of c_parm_kind,
        importing type seopardecl value '0',
        exporting type seopardecl value '1',
        changing  type seopardecl value '2',
        returning type seopardecl value '3',
      end of c_parm_kind .

    types:
      ty_parameters_tt type standard table of seosubcodf with default key .
    types:
      begin of ty_url,
        regex       type string,
        group_names type standard table of seosconame with default key,
      end of ty_url .
    types:
      begin of ty_meta,
        summary type string,
        url     type ty_url,
        method  type string,
        handler type string,
        tags    type standard table of string with default key,
      end of ty_meta .
    types:
      begin of ty_meta_internal,
        meta       type ty_meta,
        obj        type ref to object,
        parameters type ty_parameters_tt,
        classname  type seoclsname,
      end of ty_meta_internal .
    types:
      ty_meta_internal_tt type standard table of ty_meta_internal with default key .
    types:
      ty_meta_tt type standard table of ty_meta with default key .

    constants:
      begin of c_method,
        get    type string value 'GET',
        post   type string value 'POST',
        put    type string value 'PUT',
        delete type string value 'DELETE',
      end of c_method .

    methods constructor
      importing
        !ii_server       type ref to if_http_server
        !iv_base         type string
        !iv_swagger_json type string default '/swagger.json'
        !iv_swagger_html type string default '/swagger.html'
        !iv_title        type string .
    methods register
      importing
        !ii_handler type ref to zif_swag_handler .
    methods run
      raising
        cx_static_check .
  protected section.

    data mv_base type string .
    data mi_server type ref to if_http_server .
    data mt_meta type ty_meta_internal_tt .
    data mv_swagger_json type string .
    data mv_swagger_html type string .
    data mv_title type string .

    methods build_parameters
      importing
        !is_meta             type ty_meta_internal
      returning
        value(rt_parameters) type abap_parmbind_tab .
    methods create_data
      importing
        !is_meta       type ty_meta_internal
      returning
        value(rr_data) type ref to data .
    methods from_body
      importing
        !is_meta type ty_meta_internal
        !ir_ref  type ref to data .
    methods from_query
      importing
        !is_meta type ty_meta_internal
        !ir_ref  type ref to data .
    methods from_path
      importing
        !is_meta type ty_meta_internal
        !ir_ref  type ref to data .
    methods generate_spec
      importing
        !iv_title       type clike
        !iv_description type clike .
    methods generate_ui
      importing
        !iv_json_url type string
        !iv_dist     type string default ''
        !iv_title    type clike default ''
      returning
        value(rv_ui) type string .
    methods json_reply
      importing
        !is_meta       type ty_meta_internal
        !it_parameters type abap_parmbind_tab .
    methods text_reply
      importing
        !is_meta       type ty_meta_internal
        !it_parameters type abap_parmbind_tab .
    methods validate_parameters
      importing
        !it_parameters type ty_parameters_tt .
  private section.
    methods set_response_type_json.
endclass.



class zcl_swag implementation.


  method build_parameters.

    data: ls_parameter like line of rt_parameters,
          lr_dref      type ref to data.

    field-symbols: <ls_parameter> like line of is_meta-parameters,
                   <lg_comp>      type any,
                   <lg_struc>     type any.


    lr_dref = create_data( is_meta ).
    assign lr_dref->* to <lg_struc>.

    loop at is_meta-parameters assigning <ls_parameter>.
      assign component <ls_parameter>-sconame of structure <lg_struc> to <lg_comp>.
      assert sy-subrc = 0.
      ls_parameter-name  = <ls_parameter>-sconame.
      get reference of <lg_comp> into ls_parameter-value.
      insert ls_parameter into table rt_parameters.
    endloop.

    from_path( is_meta = is_meta
               ir_ref  = lr_dref ).

    if is_meta-meta-method = c_method-get.
      from_query( is_meta = is_meta
                  ir_ref  = lr_dref ).
    else.
      from_body( is_meta = is_meta
                 ir_ref  = lr_dref ).
    endif.

  endmethod.


  method constructor.

    mi_server       = ii_server.
    mv_base         = iv_base.
    mv_swagger_json = iv_swagger_json.
    mv_swagger_html = iv_swagger_html.
    mv_title        = iv_title.

  endmethod.


  method create_data.

    data: lo_struct     type ref to cl_abap_structdescr,
          lt_components type cl_abap_structdescr=>component_table,
          lo_typedescr  type ref to cl_abap_typedescr.

    field-symbols: <ls_component> like line of lt_components,
                   <ls_parameter> like line of is_meta-parameters.


    loop at is_meta-parameters assigning <ls_parameter>.
      append initial line to lt_components assigning <ls_component>.
      <ls_component>-name = <ls_parameter>-sconame.

      lo_typedescr = zcl_swag_map_type=>get_typedescr( <ls_parameter> ).

      <ls_component>-type ?= lo_typedescr.
    endloop.

    lo_struct = cl_abap_structdescr=>get( lt_components ).

    create data rr_data type handle lo_struct.

  endmethod.


  method from_body.

    data: json_body  type string,
          lo_writer type ref to cl_sxml_string_writer,
          lv_json   type xstring.

    field-symbols: <ls_parameter> like line of is_meta-parameters,
                   <lg_handler_parameter_in>      type any,
                   <lg_struc>     type any.


    assign ir_ref->* to <lg_struc>.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = c_parm_kind-importing.
      read table is_meta-meta-url-group_names from <ls_parameter>-sconame
        transporting no fields.
      if sy-subrc = 0.
* ignore parameters that are part of url
        continue.
      endif.

      assign component <ls_parameter>-sconame of structure <lg_struc> to <lg_handler_parameter_in>.
      assert sy-subrc = 0.

      if <ls_parameter>-type = 'STRING'.
        <lg_handler_parameter_in> = mi_server->request->get_cdata( ).
      elseif <ls_parameter>-type = 'XSTRING'.
        <lg_handler_parameter_in> = mi_server->request->get_data( ).
      else.
        json_body = mi_server->request->get_cdata( ).

        /ui2/cl_json=>deserialize(
          exporting
            json             = json_body
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          changing
            data             = <lg_handler_parameter_in>
        ).

      endif.

* multiple body input parameters not allowed
* todo, this should be validated earlier
      return.

    endloop.

  endmethod.


  method from_path.

    define _store.
      READ TABLE is_meta-meta-url-group_names INDEX &1 INTO lv_component.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_component OF STRUCTURE <lg_struc> TO <lg_comp>.
        ASSERT sy-subrc = 0.
        <lg_comp> = lv_match&1.
      ENDIF.
    end-of-definition.

    data: lv_path      type string,
          lv_component type string,
          lv_match1    type string,
          lv_match2    type string,
          lv_match3    type string,
          lv_match4    type string,
          lv_match5    type string.

    field-symbols: <lg_comp>  type any,
                   <lg_struc> type any.


    assign ir_ref->* to <lg_struc>.

    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).

    find regex is_meta-meta-url-regex in lv_path
      submatches lv_match1 lv_match2 lv_match3 lv_match4 lv_match5.

    _store 1.
    _store 2.
    _store 3.
    _store 4.
    _store 5.

  endmethod.


  method from_query.

    data: lv_field  type string.

    field-symbols: <ls_parameter> like line of is_meta-parameters,
                   <lg_comp>      type any,
                   <lg_struc>     type any.


    assign ir_ref->* to <lg_struc>.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = c_parm_kind-importing.
      read table is_meta-meta-url-group_names from <ls_parameter>-sconame
        transporting no fields.
      if sy-subrc = 0.
* ignore parameters that are part of url
        continue.
      endif.

      assign component <ls_parameter>-sconame of structure <lg_struc> to <lg_comp>.
      assert sy-subrc = 0.

      lv_field = to_lower( <ls_parameter>-sconame ).


      data lo_data type string.

      lo_data = mi_server->request->get_form_field( lv_field ).

    endloop.

  endmethod.


  method generate_spec.

    data: lv_spec type string,
          lo_spec type ref to zcl_swag_spec.


    create object lo_spec
      exporting
        iv_title       = iv_title
        iv_description = iv_description
        it_meta        = mt_meta
        iv_base        = mv_base.

    lv_spec = lo_spec->generate( ).

    mi_server->response->set_cdata( lv_spec ).
    set_response_type_json( ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  endmethod.


  method generate_ui.
* todo, IV_DIST not supplyed from anywhere?

    define _add.
      CONCATENATE rv_ui &1 cl_abap_char_utilities=>newline
        INTO rv_ui ##NO_TEXT.
    end-of-definition.

    _add '<!DOCTYPE html>'.
    _add '<html>'.
    _add '<head>'.
    _add '<meta charset="UTF-8">'.

    _add '<title>'.
    if iv_title is initial.
      _add 'Swagger UI'.
    else.
      _add iv_title.
    endif.
    _add '</title>'.

    _add '<link href="iv_dist/swagger-ui.css" media="screen" rel="stylesheet" type="text/css"/>'.
    _add '<style>'.
    _add '  body {'.
    _add '    margin:0;'.
    _add '    background: #fafafa;'.
    _add '  }'.
    _add '</style>'.
    _add '</head>'.
    _add '<body>'.
    _add '<div id="swagger-ui"></div>'.
    _add '<script src="iv_dist/swagger-ui-bundle.js" type="text/javascript"></script>'.
    _add '<script src="iv_dist/swagger-ui-standalone-preset.js" type="text/javascript"></script>'.
    _add '<script type="text/javascript">'.
    _add 'window.onload = function() {'.
    _add 'const ui = SwaggerUIBundle({'.
    _add '  url: "swagger.json",'.
    _add '  validatorUrl: "",'.
    _add '  dom_id: "#swagger-ui",'.
    _add '  presets: ['.
    _add '    SwaggerUIBundle.presets.apis,'.
    _add '    Array.isArray(SwaggerUIStandalonePreset) ? SwaggerUIStandalonePreset : SwaggerUIStandalonePreset.default'.
    _add '  ],'.
    _add '  plugins: ['.
    _add '    SwaggerUIBundle.plugins.DownloadUrl'.
    _add '  ],'.
    _add '  layout: "StandaloneLayout"'.
    _add '})'.
    _add 'window.ui = ui'.
    _add '}'.
    _add '</script>'.
    _add '</body>'.
    _add '</html>'.

    if iv_dist is initial.
      replace all occurrences of 'iv_dist'
        in rv_ui with 'http://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.22.2'.
    else.
      replace all occurrences of 'iv_dist'
        in rv_ui with iv_dist ##NO_TEXT.
    endif.

    replace first occurrence of 'swagger.json'
      in rv_ui with iv_json_url ##NO_TEXT.

    mi_server->response->set_cdata( rv_ui ).
    mi_server->response->set_status( code = 200 reason = '200' ).

  endmethod.


  method json_reply.

    data: lv_data   type xstring,
          lo_writer type ref to cl_sxml_string_writer.

    field-symbols: <ls_meta>      like line of is_meta-parameters,
                   <ls_parameter> like line of it_parameters,
                   <lg_handler_parameter_out>     type any.


    read table is_meta-parameters assigning <ls_meta>
      with key pardecltyp = c_parm_kind-returning.
    if sy-subrc  = 0.
      read table it_parameters assigning <ls_parameter>
        with key name = <ls_meta>-sconame.
      assert sy-subrc = 0.

      assign <ls_parameter>-value->* to <lg_handler_parameter_out>.

      data(lv_response_as_json) = /ui2/cl_json=>serialize(
        exporting
          data             = <lg_handler_parameter_out>
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).


    endif.

    mi_server->response->set_cdata( lv_response_as_json ).

  endmethod.


  method register.

    data: ls_meta like line of mt_meta,
          lt_meta type ty_meta_tt,
          lo_obj  type ref to cl_abap_objectdescr.


    lt_meta = ii_handler->meta( ).
    loop at lt_meta into ls_meta-meta.
      ls_meta-obj = ii_handler.

      lo_obj ?= cl_abap_objectdescr=>describe_by_object_ref( ii_handler ).

      read table lo_obj->methods
        with key name = ls_meta-meta-handler
        visibility = cl_abap_objectdescr=>public
        transporting no fields.
* method must exist and be public
      assert sy-subrc = 0.

      ls_meta-classname = lo_obj->absolute_name+7.

      select * from seosubcodf
        into table ls_meta-parameters
        where clsname = ls_meta-classname
        and cmpname = ls_meta-meta-handler
        and sconame not like 'ZCX_%'
        order by primary key.                             "#EC CI_SUBRC
      assert sy-subrc = 0. " the method does not have any parameters

      validate_parameters( ls_meta-parameters ).

      append ls_meta to mt_meta.

      clear ls_meta.
    endloop.

  endmethod.


  method run.

    data: lv_path       type string,
          lv_method     type string,
          lt_parameters type abap_parmbind_tab.

    field-symbols: <ls_meta> like line of mt_meta.


    lv_path = mi_server->request->get_header_field( '~path' ).
    lv_path = cl_http_utility=>unescape_url( lv_path ).
    lv_method = mi_server->request->get_method( ).

    if lv_path = mv_base && mv_swagger_html.
      generate_ui(
        iv_json_url = mv_base && mv_swagger_json
        iv_title    = mv_title && ' - Swagger' ).
      return.
    elseif lv_path = mv_base && mv_swagger_json.
      generate_spec(
        iv_title       = mv_title
        iv_description = mv_title && ' REST functions' ).
      return.
    endif.

    loop at mt_meta assigning <ls_meta> where meta-method = lv_method.

      find regex <ls_meta>-meta-url-regex in lv_path.
      if sy-subrc = 0.

        lt_parameters = build_parameters( <ls_meta> ).
        call method <ls_meta>-obj->(<ls_meta>-meta-handler)
          parameter-table lt_parameters.

        mi_server->response->set_compression( ).

        loop at <ls_meta>-parameters
            transporting no fields
            where pardecltyp = c_parm_kind-returning
            and ( type = 'STRING' or type = 'XSTRING' ).
* assumption: RETURNING only, no EXPORTING at the same time
          exit.
        endloop.
        if sy-subrc = 0.
          text_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        else.
          set_response_type_json( ).
          json_reply( is_meta       = <ls_meta>
                      it_parameters = lt_parameters ).
        endif.

        mi_server->response->set_header_field( name  = 'cache-control'
                                               value = 'no-cache' ).

        mi_server->response->set_status( code = 200 reason = '200' ).
        return.

      endif.
    endloop.

    "@Todo: allow sicf/enforce SICF handling of error
    mi_server->response->set_cdata( '404, swagger' ).
    mi_server->response->set_status( code = 404 reason = '404' ).

  endmethod.


  method text_reply.

    field-symbols: <lg_any>       type any,
                   <ls_meta>      like line of is_meta-parameters,
                   <ls_parameter> like line of it_parameters.


    read table is_meta-parameters assigning <ls_meta>
      with key pardecltyp = c_parm_kind-returning.
    if sy-subrc  = 0.
      read table it_parameters assigning <ls_parameter>
        with key name = <ls_meta>-sconame.
      assert sy-subrc = 0.

      assign <ls_parameter>-value->* to <lg_any>.

      case <ls_meta>-type.
        when 'XSTRING'.
          mi_server->response->set_data( <lg_any> ).
        when 'STRING'.
          mi_server->response->set_cdata( <lg_any> ).
        when others.
          assert 0 = 1.
      endcase.
    endif.

  endmethod.


  method validate_parameters.

* no EXPORTING, no CHANGING
    loop at it_parameters transporting no fields
        where pardecltyp = c_parm_kind-exporting
        or pardecltyp = c_parm_kind-changing.
      assert 0 = 1.
    endloop.

* no reference types
* todo

* todo, max one importing parameter? apart from path parameters?

  endmethod.

  method set_response_type_json.

    mi_server->response->set_header_field( name  = 'content-type'
                                           value = 'application/json' ).

  endmethod.

endclass.
