CLASS zcl_swag_spec DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_title          TYPE clike
        !iv_description    TYPE clike
        !it_meta           TYPE zcl_swag=>ty_meta_internal_tt
        !iv_base           TYPE clike
        !it_tagdescription TYPE zcl_swag=>ty_tagdescription_tt .
    METHODS generate
      RETURNING
        VALUE(rv_spec) TYPE string .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_path,
        path TYPE string,
        meta TYPE zcl_swag=>ty_meta_internal_tt,
      END OF ty_path .
    TYPES:
      ty_path_tt TYPE STANDARD TABLE OF ty_path WITH DEFAULT KEY .

    DATA mv_title TYPE string .
    DATA mv_description TYPE string .
    DATA mt_meta TYPE zcl_swag=>ty_meta_internal_tt .
    DATA mv_base TYPE string .
    DATA mt_definitions TYPE string_table .
    DATA mt_tagdescription TYPE zcl_swag=>ty_tagdescription_tt .

    METHODS handle_paths
      RETURNING
        VALUE(rt_paths) TYPE ty_path_tt .
    METHODS request
      IMPORTING
        !is_meta      TYPE zcl_swag=>ty_meta_internal
        !is_parameter TYPE seosubcodf .
    METHODS definitions
      RETURNING
        VALUE(rv_defs) TYPE string .
    METHODS path
      IMPORTING
        !is_meta       TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS parameters
      IMPORTING
        !is_meta             TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_parameters) TYPE string .
    METHODS response
      IMPORTING
        !is_meta           TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_response) TYPE string .
    METHODS tagdescriptions
      RETURNING
        VALUE(rv_defs) TYPE string .

  PRIVATE SECTION.

    METHODS strip_quotes IMPORTING iv_string        TYPE csequence
                         RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.



CLASS zcl_swag_spec IMPLEMENTATION.


  METHOD constructor.

    mv_title       = iv_title.
    mv_description = iv_description.
    mt_meta        = it_meta.
    mv_base        = iv_base.
    mt_tagdescription = it_tagdescription.

  ENDMETHOD.


  METHOD definitions.

    DATA: lv_string TYPE string,
          lv_sep    TYPE string,
          lt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    SORT mt_definitions.
    DELETE ADJACENT DUPLICATES FROM mt_definitions.

    APPEND '  "definitions":{' TO lt_string.

    lv_sep = |,\n|.
    CONCATENATE LINES OF mt_definitions INTO lv_string SEPARATED BY lv_sep.
    APPEND lv_string TO lt_string.

    APPEND '  }' TO lt_string.

    CONCATENATE LINES OF lt_string INTO rv_defs
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD generate.

    DEFINE _add.
      CONCATENATE rv_spec &1 cl_abap_char_utilities=>newline
         INTO rv_spec ##NO_TEXT.
    END-OF-DEFINITION.

    DATA: lt_paths     TYPE TABLE OF ty_path,
          lv_last_path TYPE abap_bool,
          lv_last_meta TYPE abap_bool,
          lv_tags      TYPE string,
          lv_add       TYPE string.

    FIELD-SYMBOLS: <ls_path> LIKE LINE OF lt_paths,
                   <ls_meta> LIKE LINE OF mt_meta.

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
    _add '    "https",'.
    _add '    "http"'.
    _add '  ],'.
    _add '  "consumes":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "produces":['.
    _add '    "application/json"'.
    _add '  ],'.
    _add '  "paths":{'.

    lt_paths = handle_paths( ).

    LOOP AT lt_paths ASSIGNING <ls_path>.
      lv_last_path = boolc( sy-tabix = lines( lt_paths ) ).

      lv_add = |    "{ <ls_path>-path }":\{|.
      _add lv_add.

      LOOP AT <ls_path>-meta ASSIGNING <ls_meta>.
        lv_last_meta = boolc( sy-tabix = lines( <ls_path>-meta ) ).

        lv_add = |      "{ to_lower( <ls_meta>-meta-method ) }":\{|.
        _add lv_add.

        IF lines( <ls_meta>-meta-tags ) > 0.
          CONCATENATE LINES OF <ls_meta>-meta-tags INTO lv_tags SEPARATED BY '", "'.
          lv_add = |        "tags":["{ lv_tags }"],|.
          _add lv_add.
        ENDIF.

        lv_add = |        "summary":"{ <ls_meta>-meta-summary }",|.
        _add lv_add.
        lv_add = |        "description":"",|.
        _add lv_add.

        lv_add = parameters( <ls_meta> ).
        _add lv_add.

        _add '        "produces":['.

        READ TABLE <ls_meta>-parameters WITH KEY
          pardecltyp = zcl_swag=>c_parm_kind-returning
          type = 'STRING' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          _add '"text/plain"'.
        ELSE.
          _add '"application/json"'.
        ENDIF.

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
    lv_add = tagdescriptions( ).
    _add lv_add.
    lv_add = definitions( ).
    _add lv_add.
    _add '}'.

  ENDMETHOD.


  METHOD handle_paths.

    DATA lv_path      TYPE string.

    FIELD-SYMBOLS: <ls_path> LIKE LINE OF rt_paths,
                   <ls_meta> LIKE LINE OF mt_meta.

* handle path with multiple handlers(ie. GET and POST)
    LOOP AT mt_meta ASSIGNING <ls_meta>.
      lv_path = path( <ls_meta> ).
      READ TABLE rt_paths ASSIGNING <ls_path> WITH KEY path = lv_path.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_paths ASSIGNING <ls_path>.
        <ls_path>-path = lv_path.
      ENDIF.
      APPEND <ls_meta> TO <ls_path>-meta.
    ENDLOOP.

  ENDMETHOD.


  METHOD parameters.

    DATA: lt_string TYPE TABLE OF string,
          lv_string LIKE LINE OF lt_string,
          lv_type   TYPE string,
          lo_map    TYPE REF TO zcl_swag_map_type.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters.


    APPEND '"parameters":[' TO lt_string.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = zcl_swag=>c_parm_kind-importing.
      APPEND '{' TO lt_string.

      CONCATENATE '"name":"' <ls_parameter>-sconame '",' INTO lv_string.
      APPEND lv_string TO lt_string.

      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND '"in":"path",' TO lt_string.
      ELSEIF is_meta-meta-method = zcl_swag=>c_method-get.
        APPEND '"in":"query",' TO lt_string.
      ELSE.
        APPEND '"in":"body",' TO lt_string.
        lv_string = |"schema": \{"$ref": "#/definitions/{ is_meta-meta-handler }_Request"\}|.
        APPEND lv_string TO lt_string.
        APPEND '},' TO lt_string.
        request( is_meta     = is_meta
                 is_parameter = <ls_parameter> ).
        CONTINUE.
      ENDIF.

      APPEND '"description":"",' TO lt_string.

      CREATE OBJECT lo_map
        EXPORTING
          is_param = <ls_parameter>.
      lv_type = lo_map->map( ).
      CONCATENATE lv_type ',' INTO lv_string.
      APPEND lv_string TO lt_string.

      IF NOT <ls_parameter>-parvalue IS INITIAL.
        lv_string = |"default":"{ strip_quotes( <ls_parameter>-parvalue ) }",|.
        APPEND lv_string TO lt_string.
      ENDIF.

      IF <ls_parameter>-paroptionl = abap_true OR NOT <ls_parameter>-parvalue IS INITIAL.
        APPEND '"required":false' TO lt_string.
      ELSE.
        APPEND '"required":true' TO lt_string.
      ENDIF.

      APPEND '},' TO lt_string.
    ENDLOOP.
    IF sy-subrc = 0.
* fix the comma
      DELETE lt_string INDEX lines( lt_string ).
      APPEND '}' TO lt_string.
    ENDIF.

    APPEND '],' TO lt_string.

    CONCATENATE LINES OF lt_string INTO rv_parameters
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD path.

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


  METHOD request.

    DATA: lo_map    TYPE REF TO zcl_swag_map_type,
          lv_string TYPE string,
          lv_type   TYPE string.


    CREATE OBJECT lo_map
      EXPORTING
        is_param  = is_parameter
        iv_schema = abap_false.
    lv_type = lo_map->map( ).

* todo, basic/simple types?
    lv_string = |"{ is_parameter-type }": \{{ lv_type }\}|.
    APPEND lv_string TO mt_definitions.

  ENDMETHOD.


  METHOD response.

    DATA: lt_string TYPE TABLE OF string,
          lv_type   TYPE string,
          lv_string TYPE string,
          lo_map    TYPE REF TO zcl_swag_map_type.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters.


    APPEND '"200": {' TO lt_string.

    READ TABLE is_meta-parameters WITH KEY pardecltyp = zcl_swag=>c_parm_kind-returning
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      APPEND '  "description": "successful operation",' TO lt_string.
    ELSE.
      APPEND '  "description": "successful operation"' TO lt_string.
    ENDIF.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = zcl_swag=>c_parm_kind-returning.
      CREATE OBJECT lo_map
        EXPORTING
          is_param  = <ls_parameter>
          iv_schema = abap_false.
      lv_type = lo_map->map( ).

      APPEND '"schema": {' TO lt_string.
      lv_string = |"$ref": "#/definitions/{ <ls_parameter>-type }"|.
      APPEND lv_string TO lt_string.
      APPEND '}' TO lt_string.

      lv_string = |"{ <ls_parameter>-type }":\{"type": "object","properties": \{"DATA": \{{ lv_type }\}\}\}|.
      APPEND lv_string TO mt_definitions.
    ENDLOOP.

    APPEND '},' TO lt_string.

    CONCATENATE LINES OF lt_string INTO rv_response
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD tagdescriptions.

    DATA: lv_string TYPE string,
          lv_sep    TYPE string,
          lt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_tagdescription> LIKE LINE OF mt_tagdescription.


    IF mt_tagdescription IS NOT INITIAL.

      APPEND '  "tags": [' TO lt_string.

      LOOP AT mt_tagdescription ASSIGNING <ls_tagdescription>.
        APPEND '{' TO lt_string.
        CONCATENATE '"name":"' <ls_tagdescription>-tag '",' INTO lv_string.
        APPEND lv_string TO lt_string.
        CONCATENATE '"description":"' <ls_tagdescription>-description '"' INTO lv_string.
        APPEND lv_string TO lt_string.

        IF <ls_tagdescription>-externaldoc IS NOT INITIAL.
          APPEND ',' TO lt_string.
          CONCATENATE '"externalDocs": { "description":"' <ls_tagdescription>-externaldoc-description '",'
            INTO lv_string.
          APPEND lv_string TO lt_string.
          CONCATENATE '"url":"' <ls_tagdescription>-externaldoc-url '" }' INTO lv_string.
          APPEND lv_string TO lt_string.
        ENDIF.

        APPEND '}' TO lt_string.
        APPEND ',' TO lt_string.
      ENDLOOP.
      IF sy-subrc = 0.
* fix the comma
        DELETE lt_string INDEX lines( lt_string ).
        APPEND '],' TO lt_string.
      ENDIF.

      CONCATENATE LINES OF lt_string INTO rv_defs
        SEPARATED BY cl_abap_char_utilities=>newline.

    ENDIF.

  ENDMETHOD.

  METHOD strip_quotes.
    FIND REGEX `['|``](.*)['|``]` IN iv_string SUBMATCHES rv_result.
    IF rv_result IS INITIAL.
      rv_result = iv_string.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
