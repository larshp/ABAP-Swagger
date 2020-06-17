class zcl_dummy_swagger_handler definition create public
for testing
public.

  public section.
    interfaces zif_swag_handler.
    types:
      begin of ty_structure,
        foo type string,
        bar type string,
        foo_bar type string,
      end of ty_structure.

    methods the_real_stuff
      importing
        !iv_foo        type string optional
        !iv_bar        type string optional
      returning
        value(rs_data) type ty_structure.
    methods
      set_meta
        importing
          it_meta type zcl_swag=>ty_meta_tt.
  protected section.
  private section.
    data mt_meta type zcl_swag=>ty_meta_tt.

endclass.

class zcl_dummy_swagger_handler implementation.

  method the_real_stuff.

    concatenate iv_foo iv_bar into rs_data-foo.
    rs_data-bar = iv_bar.
    rs_data-foo_bar = 'FOO_BAR'.

  endmethod.


  method zif_swag_handler~meta.

    rt_meta = mt_meta.

  endmethod.

  method set_meta.
    mt_meta = it_meta.
  endmethod.

endclass.
