class zcl_swag_spec definition
  public
  create public .

  public section.

    methods constructor
      importing
        !iv_title       type clike
        !iv_description type clike
        !it_meta        type zcl_swag=>ty_meta_internal_tt
        !iv_base        type clike .
    methods generate
      returning
        value(rv_spec) type string .
  protected section.

    data mv_title type string .
    data mv_description type string .
    data mt_meta type zcl_swag=>ty_meta_internal_tt .
    data mv_base type string .
    data mt_definitions type string_table .

    methods definitions
      returning
        value(rv_defs) type string .
    methods path
      importing
        !is_meta       type zcl_swag=>ty_meta_internal
      returning
        value(rv_path) type string .
    methods parameters
      importing
        !is_meta             type zcl_swag=>ty_meta_internal
      returning
        value(rv_parameters) type string .
    methods response
      importing
        !is_meta           type zcl_swag=>ty_meta_internal
      returning
        value(rv_response) type string .
  private section.
endclass.



class zcl_swag_spec implementation.


  method constructor.

    mv_title       = iv_title.
    mv_description = iv_description.
    mt_meta        = it_meta.
    mv_base        = iv_base.

  endmethod.


  method definitions.

    data: lv_string type string,
          lv_sep    type string,
          lt_string type standard table of string with default key.


    append '  "definitions":{' to lt_string.

    lv_sep = |,\n|.
    concatenate lines of mt_definitions into lv_string separated by lv_sep.
    append lv_string to lt_string.

    append '  }' to lt_string.

    concatenate lines of lt_string into rv_defs
      separated by cl_abap_char_utilities=>newline.

  endmethod.


  method generate.

    define _add.
      CONCATENATE rv_spec &1 cl_abap_char_utilities=>newline
        INTO rv_spec ##NO_TEXT.
    end-of-definition.

    types: begin of ty_path,
             path type string,
             meta like mt_meta,
           end of ty_path.

    data: lv_index     type i,
          lt_paths     type table of ty_path,
          lv_last_path type abap_bool,
          lv_last_meta type abap_bool,
          lv_path      type string,
          lv_tags      type string,
          lv_add       type string.

    field-symbols: <ls_path> like line of lt_paths,
                   <ls_meta> like line of mt_meta.


* handle path with multiple handlers(ie. GET and POST)
    loop at mt_meta assigning <ls_meta>.
      lv_path = path( <ls_meta> ).
      read table lt_paths assigning <ls_path> with key path = lv_path.
      if sy-subrc <> 0.
        append initial line to lt_paths assigning <ls_path>.
        <ls_path>-path = lv_path.
      endif.
      append <ls_meta> to <ls_path>-meta.
    endloop.

    _add '{'.
    _add '  "swagger":"2.0",'.
    _add '  "info":{  '.
    _add '    "version":"1.0.0",'.

    lv_add = |    "title":"{ mv_title }",|.
    _add lv_add.
    lv_add = |    "description":"{ mv_description }"|.
    _add lv_add.

    _add '  },'.

    lv_add = |  "basePath":"{ mv_base }",|.
    _add lv_add.

    _add '  "securityDefinitions": {'.
    _add '    "basicAuth": {"type": "basic"}'.
    _add '  },'.
    _add '  "security": ['.
    _add '    {"basicAuth": []}'.
    _add '  ],'.

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

    loop at lt_paths assigning <ls_path>.
      lv_last_path = boolc( sy-tabix = lines( lt_paths ) ).

      lv_add = |    "{ <ls_path>-path }":\{|.
      _add lv_add.

      loop at <ls_path>-meta assigning <ls_meta>.
        lv_last_meta = boolc( sy-tabix = lines( <ls_path>-meta ) ).

        lv_add = |      "{ to_lower( <ls_meta>-meta-method ) }":\{|.
        _add lv_add.

        if lines( <ls_meta>-meta-tags ) > 0.
          concatenate lines of <ls_meta>-meta-tags into lv_tags separated by '", "'.
          lv_add = |        "tags":["{ lv_tags }"],|.
          _add lv_add.
        endif.

        lv_add = |        "summary":"{ <ls_meta>-meta-summary }",|.
        _add lv_add.
        lv_add = |        "description":"",|.
        _add lv_add.

        lv_add = parameters( <ls_meta> ).
        _add lv_add.

        _add '        "produces":['.

        read table <ls_meta>-parameters with key
          pardecltyp = zcl_swag=>c_parm_kind-returning
          type = 'STRING' transporting no fields.
        if sy-subrc = 0.
          _add '"text/plain"'.
        else.
          _add '"application/json"'.
        endif.

        _add '        ],'.
        _add '        "responses":{'.

        lv_add = response( <ls_meta> ).
        _add lv_add.

        _add '          "401":{'.
        _add '            "description":"not authorized"'.
        _add '          },'.
        _add '          "404":{'.
        _add '            "description":"not found"'.
        _add '          },'.
        _add '          "500":{'.
        _add '            "description":"error"'.
        _add '          }'.
        _add '        }'.
        _add '      }'.

        if lv_last_meta = abap_false.
          _add ','.
        endif.
      endloop.

      _add '    }'.
      if lv_last_path = abap_false.
        _add ','.
      endif.
    endloop.

    _add '  },'.

    lv_add = definitions( ).
    _add lv_add.

    _add '}'.

  endmethod.


  method parameters.

    data: lt_string type table of string,
          ls_string like line of lt_string,
          lv_type   type string,
          lo_map    type ref to zcl_swag_map_type.

    field-symbols: <ls_parameter> like line of is_meta-parameters.


    append '"parameters":[' to lt_string.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = zcl_swag=>c_parm_kind-importing.

      append '{' to lt_string.

      data(lv_camel_cased) = /ui2/cl_json=>serialize(
          data             =  <ls_parameter>-sconame
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      ).
      concatenate '"name":"' lv_camel_cased '",' into ls_string.
      append ls_string to lt_string.

      read table is_meta-meta-url-group_names from <ls_parameter>-sconame
        transporting no fields.
      if sy-subrc = 0.
        append '"in":"path",' to lt_string.
      elseif is_meta-meta-method = zcl_swag=>c_method-get.
        append '"in":"query",' to lt_string.
      else.
        append '"in":"body",' to lt_string.
      endif.

      append '"description":"",' to lt_string.

      create object lo_map
        exporting
          is_param = <ls_parameter>.
      lv_type = lo_map->map( ).
      concatenate lv_type ',' into ls_string.
      append ls_string to lt_string.

      if <ls_parameter>-paroptionl = abap_true.
        append '"required":false' to lt_string.
      else.
        append '"required":true' to lt_string.
      endif.

      append '},' to lt_string.
    endloop.
    if sy-subrc = 0.
* fix the comma
      delete lt_string index lines( lt_string ).
      append '}' to lt_string.
    endif.

    append '],' to lt_string.

    concatenate lines of lt_string into rv_parameters
      separated by cl_abap_char_utilities=>newline.

  endmethod.


  method path.

    data: lv_name    type string,
          lv_offset1 type i,
          lv_offset2 type i.


    rv_path = is_meta-meta-url-regex.

    replace all occurrences of '$' in rv_path with ''.

* replace the regex groups like (\w*) with swagger identifies {IV_NAME}
    loop at is_meta-meta-url-group_names into lv_name.
      find first occurrence of '(' in rv_path match offset lv_offset1.
      find first occurrence of ')' in rv_path match offset lv_offset2.
      lv_offset2 = lv_offset2 + 1.
      concatenate rv_path(lv_offset1) '{' lv_name '}' rv_path+lv_offset2 into rv_path.
    endloop.

  endmethod.


  method response.

    data: lt_string type table of string,
          lv_type   type string,
          lv_string type string,
          lo_map    type ref to zcl_swag_map_type.

    field-symbols: <ls_parameter> like line of is_meta-parameters.


    append '"200": {' to lt_string.

    read table is_meta-parameters with key pardecltyp = zcl_swag=>c_parm_kind-returning
      transporting no fields.
    if sy-subrc = 0.
      append '  "description": "successful operation",' to lt_string.
    else.
      append '  "description": "successful operation"' to lt_string.
    endif.

    loop at is_meta-parameters assigning <ls_parameter>
        where pardecltyp = zcl_swag=>c_parm_kind-returning.
      create object lo_map
        exporting
          is_param  = <ls_parameter>
          iv_schema = abap_false.
      lv_type = lo_map->map( ).

      append '"schema": {' to lt_string.
      lv_string = |"$ref": "#/definitions/{ is_meta-meta-handler }_Response"|.
      append lv_string to lt_string.
      append '}' to lt_string.

      lv_string = |"{ is_meta-meta-handler }_Response":\{"type": "object","properties": \{"DATA": \{{ lv_type }\}\}\}|.
      append lv_string to mt_definitions.
    endloop.

    append '},' to lt_string.

    concatenate lines of lt_string into rv_response
      separated by cl_abap_char_utilities=>newline.

  endmethod.
endclass.
