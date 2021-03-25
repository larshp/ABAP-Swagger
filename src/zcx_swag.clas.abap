CLASS zcx_swag DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    DATA:
      status_code TYPE i READ-ONLY,
      text1       TYPE string READ-ONLY,
      text2       TYPE string READ-ONLY,
      text3       TYPE string READ-ONLY,
      text4       TYPE string READ-ONLY.

    CLASS-METHODS raise_system
      RAISING
        zcx_swag.

    CLASS-METHODS raise_text
      IMPORTING
        text TYPE csequence
      RAISING
        zcx_swag.

    METHODS constructor
      IMPORTING
        !status_code TYPE i OPTIONAL
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !text1       TYPE string OPTIONAL
        !text2       TYPE string OPTIONAL
        !text3       TYPE string OPTIONAL
        !text4       TYPE string OPTIONAL
        !previous    LIKE previous OPTIONAL.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_t100_message,
        key   TYPE scx_t100key,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ty_t100_message.

    CLASS-METHODS text_to_t100key
      IMPORTING
        text             TYPE csequence
      RETURNING
        VALUE(rs_result) TYPE ty_t100_message.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_swag IMPLEMENTATION.


  METHOD constructor ##adt_suppress_generation.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    IF status_code IS INITIAL.
      me->status_code = 500.
    ELSE.
      me->status_code = status_code.
    ENDIF.

  ENDMETHOD.


  METHOD raise_system.

    DATA:
      ls_textid TYPE scx_t100key,
      lv_text1  TYPE string,
      lv_text2  TYPE string,
      lv_text3  TYPE string,
      lv_text4  TYPE string.

    ls_textid-msgid = sy-msgid.
    ls_textid-msgno = sy-msgno.
    ls_textid-attr1 = 'TEXT1'.
    ls_textid-attr2 = 'TEXT2'.
    ls_textid-attr3 = 'TEXT3'.
    ls_textid-attr4 = 'TEXT4'.

    lv_text1 = sy-msgv1.
    lv_text1 = sy-msgv2.
    lv_text1 = sy-msgv3.
    lv_text1 = sy-msgv4.

    RAISE EXCEPTION TYPE zcx_swag
      EXPORTING
        textid = ls_textid
        text1  = lv_text1
        text2  = lv_text2
        text3  = lv_text3
        text4  = lv_text4.

  ENDMETHOD.


  METHOD raise_text.

    DATA:
      ls_t100_message TYPE ty_t100_message,
      lv_text1        TYPE string,
      lv_text2        TYPE string,
      lv_text3        TYPE string,
      lv_text4        TYPE string.

    ls_t100_message = text_to_t100key( text ).

    RAISE EXCEPTION TYPE zcx_swag
      EXPORTING
        textid = ls_t100_message-key
        text1  = lv_text1
        text2  = lv_text2
        text3  = lv_text3
        text4  = lv_text4.

  ENDMETHOD.


  METHOD text_to_t100key.

    DATA:
      BEGIN OF ls_message_variables,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ls_message_variables.

    ls_message_variables = text.

    rs_result-key-msgid = '00'.
    rs_result-key-msgno = '001'.
    rs_result-key-attr1 = 'TEXT1'.
    rs_result-key-attr2 = 'TEXT2'.
    rs_result-key-attr3 = 'TEXT3'.
    rs_result-key-attr4 = 'TEXT4'.

    rs_result-msgv1 = ls_message_variables-msgv1.
    rs_result-msgv2 = ls_message_variables-msgv2.
    rs_result-msgv3 = ls_message_variables-msgv3.
    rs_result-msgv4 = ls_message_variables-msgv4.

  ENDMETHOD.


ENDCLASS.
