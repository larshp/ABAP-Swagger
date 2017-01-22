INTERFACE zif_swag_handler
  PUBLIC.

  METHODS meta
    RETURNING
      VALUE(rt_meta) TYPE zcl_swag=>ty_meta_tt.
ENDINTERFACE.
