CLASS zcl_dummy_swagger_handler DEFINITION CREATE PUBLIC
  FOR TESTING
  PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_swag_handler.
    TYPES:
      BEGIN OF ty_structure,
        foo     TYPE string,
        bar     TYPE string,
        foo_bar TYPE string,
      END OF ty_structure.

    METHODS the_real_stuff
      IMPORTING
        !iv_foo        TYPE string OPTIONAL
        !iv_bar        TYPE string OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE ty_structure.
    METHODS
      set_meta
        IMPORTING
          it_meta TYPE zcl_swag=>ty_meta_tt.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_meta TYPE zcl_swag=>ty_meta_tt.

ENDCLASS.

CLASS zcl_dummy_swagger_handler IMPLEMENTATION.

  METHOD the_real_stuff.

    CONCATENATE iv_foo iv_bar INTO rs_data-foo.
    rs_data-bar = iv_bar.
    rs_data-foo_bar = 'FOO_BAR'.

  ENDMETHOD.


  METHOD zif_swag_handler~meta.

    rt_meta = mt_meta.

  ENDMETHOD.

  METHOD set_meta.
    mt_meta = it_meta.
  ENDMETHOD.

ENDCLASS.
