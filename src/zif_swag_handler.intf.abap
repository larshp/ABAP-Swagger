INTERFACE zif_swag_handler PUBLIC.

  METHODS meta
    RETURNING
      VALUE(rs_meta) TYPE zcl_swag=>ty_meta.

ENDINTERFACE.