CLASS zcl_swag_spec DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_title       TYPE clike
        !iv_description TYPE clike
        !it_meta        TYPE zcl_swag=>ty_meta_internal_tt
        !iv_base        TYPE clike .
    METHODS generate
      RETURNING
        VALUE(rv_spec) TYPE string .
  PROTECTED SECTION.

    DATA mv_title TYPE string .
    DATA mv_description TYPE string .
    DATA mt_meta TYPE zcl_swag=>ty_meta_internal_tt .
    DATA mv_base TYPE string .

    METHODS spec_path
      IMPORTING
        !is_meta       TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS spec_parameters
      IMPORTING
        !is_meta             TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_parameters) TYPE string .
    METHODS spec_response
      IMPORTING
        !is_meta           TYPE zcl_swag=>ty_meta_internal
      RETURNING
        VALUE(rv_response) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_SPEC IMPLEMENTATION.


  METHOD constructor.

    mv_title       = iv_title.
    mv_description = iv_description.
    mt_meta        = it_meta.
    mv_base        = iv_base.

  ENDMETHOD.


  METHOD generate.

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

    lv_add = |    "title":"{ mv_title }",|.
    _add lv_add.
    lv_add = |    "description":"{ mv_description }"|.
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
          pardecltyp = zcl_swag=>c_parm_kind-returning
          type = 'STRING' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          _add '"text/plain"'.
        ELSE.
          _add '"application/json"'.
        ENDIF.

        _add '        ],'.
        _add '        "responses":{'.

        lv_add = spec_response( <ls_meta> ).
        _add lv_add.

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

  ENDMETHOD.


  METHOD spec_parameters.

    DATA: lt_string TYPE TABLE OF string,
          ls_string LIKE LINE OF lt_string,
          lv_type   TYPE string,
          lo_map    TYPE REF TO zcl_swag_map_type.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF is_meta-parameters.


    APPEND '"parameters":[' TO lt_string.

    LOOP AT is_meta-parameters ASSIGNING <ls_parameter>
        WHERE pardecltyp = zcl_swag=>c_parm_kind-importing.

      APPEND '{' TO lt_string.

      CONCATENATE '"name":"' <ls_parameter>-sconame '",' INTO ls_string.
      APPEND ls_string TO lt_string.

      READ TABLE is_meta-meta-url-group_names FROM <ls_parameter>-sconame
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND '"in":"path",' TO lt_string.
      ELSEIF is_meta-meta-method = zcl_swag=>c_method-get.
        APPEND '"in":"query",' TO lt_string.
      ELSE.
        APPEND '"in":"body",' TO lt_string.
      ENDIF.

      APPEND '"description":"",' TO lt_string.

      CREATE OBJECT lo_map.
      lv_type = lo_map->map( <ls_parameter> ).
      CONCATENATE lv_type ',' INTO ls_string.
      APPEND ls_string TO lt_string.

      APPEND '"required":true'   TO lt_string.
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


  METHOD spec_response.

    DATA: lt_string TYPE TABLE OF string,
          lv_type   TYPE string,
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
*      CREATE OBJECT lo_map.
*      lv_type = lo_map->map( <ls_parameter> ).
*      APPEND lv_type TO lt_string.

* todo, this is wrong
      APPEND '"schema":{' TO lt_string.
      APPEND '"type":"array"' TO lt_string.
      APPEND '}' TO lt_string.
    ENDLOOP.

    APPEND '},' TO lt_string.

    CONCATENATE LINES OF lt_string INTO rv_response
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.
ENDCLASS.
