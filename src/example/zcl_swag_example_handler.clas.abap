class ZCL_SWAG_EXAMPLE_HANDLER definition
  public
  create public .

public section.

  interfaces ZIF_SWAG_HANDLER .

  types:
    BEGIN OF ty_structure,
             foo TYPE string,
             bar TYPE string,
           END OF ty_structure .

  methods THE_REAL_STUFF
    importing
      !IV_FOO type STRING optional
      !IV_BAR type STRING optional
    returning
      value(RS_DATA) type TY_STRUCTURE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SWAG_EXAMPLE_HANDLER IMPLEMENTATION.


  METHOD the_real_stuff.

    BREAK-POINT.

    rs_data-foo = 'data'.

  ENDMETHOD.


  METHOD zif_swag_handler~meta.

    DATA: lv_method TYPE string.


    rs_meta-summary = 'summary text'.
    rs_meta-description = 'this is the description'.

    rs_meta-url-regex = '/swag/(\w*)/'.
    APPEND 'IV_FOO' TO rs_meta-url-group_names.

    rs_meta-method = 'GET'.

    rs_meta-handler = 'THE_REAL_STUFF'.

  ENDMETHOD.
ENDCLASS.