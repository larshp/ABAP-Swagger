CLASS zcl_swag_example_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_swag_handler.

    TYPES:
      BEGIN OF ty_structure,
        foo TYPE string,
        bar TYPE string,
      END OF ty_structure.

    METHODS the_real_stuff
      IMPORTING
        !iv_foo        TYPE string OPTIONAL
        !iv_bar        TYPE string OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE ty_structure.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SWAG_EXAMPLE_HANDLER IMPLEMENTATION.


  METHOD the_real_stuff.

    CONCATENATE iv_foo iv_bar INTO rs_data-foo.
    rs_data-bar = iv_bar.

  ENDMETHOD.


  METHOD zif_swag_handler~meta.

    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF rt_meta.


    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'this is the description'(001).
    <ls_meta>-url-regex = '/swag/(\w*)/'.
    APPEND 'IV_FOO' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'THE_REAL_STUFF'.

  ENDMETHOD.
ENDCLASS.
