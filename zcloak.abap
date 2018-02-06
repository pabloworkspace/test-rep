*&------------------------------------------------------------------*
*& Report  ZCLOAK
*&
*&------------------------------------------------------------------*
*&
*&
*&------------------------------------------------------------------*

REPORT  zcloak.

TYPES:
  tty_key TYPE TABLE OF int1,
  tty_state TYPE TABLE OF int1,
  tty_out TYPE TABLE OF int1,
  tty_msg TYPE TABLE OF int1,
  ty_line(80) TYPE c,
  tty_line TYPE TABLE OF ty_line.

CONSTANTS:
  gv_source_start_tag TYPE ty_line VALUE '*----<< SOURCE START >>----',
  gv_source_end_tag TYPE ty_line VALUE '*-----<< SOURCE END >>-----'.

PARAMETERS: password TYPE char128 LOWER CASE.

START-OF-SELECTION.
  PERFORM sos.


*&------------------------------------------------------------------*
*&      Form  ksa
*&------------------------------------------------------------------*
*   Key Scheduling Algorithm
*-------------------------------------------------------------------*
*      -->S          text
*-------------------------------------------------------------------*
FORM ksa USING it_key TYPE tty_key
               it_msg TYPE tty_msg
      CHANGING et_state TYPE tty_state.

  STATICS:
    lv_i TYPE int1,
    lv_j TYPE int1.

  DATA:
    lv_state TYPE LINE OF tty_state,
    lv_j2 TYPE int2,
    lv_i2 TYPE int2,
    lv_swap1 TYPE LINE OF tty_state,
    lv_swap2 TYPE LINE OF tty_state,
    lv_key_index TYPE int1,
    lv_key_index2 TYPE int2,
    lv_key TYPE LINE OF tty_key,
    lv_key_len TYPE int1,
    lv_msg_len TYPE int1.

  DESCRIBE TABLE it_key LINES lv_key_len.
  DO 256 TIMES.
    lv_state = sy-index - 1.
    APPEND lv_state TO et_state.
  ENDDO.

  lv_j = 0.
  DESCRIBE TABLE it_msg LINES lv_msg_len.
  DO lv_msg_len TIMES.
    lv_i = sy-index - 1.

*    state[i]
    lv_i2 = lv_i + 1.
    READ TABLE et_state INDEX lv_i2 INTO lv_state.

*    i mod key_len
    lv_key_index = lv_i MOD lv_key_len.
    lv_key_index2 = lv_key_index + 1.

*    key[i mod key_len]
    READ TABLE it_key INDEX lv_key_index2 INTO lv_key.

    lv_j2 = lv_j + lv_state + lv_key.
    lv_j = lv_j2 MOD 256.

*    Swap S[i] and S[j]
    lv_i2 = lv_i + 1.
    READ TABLE et_state INDEX lv_i2 INTO lv_swap1.
    lv_j2 = lv_j + 1.
    READ TABLE et_state INDEX lv_j2 INTO lv_swap2.
    MODIFY et_state INDEX lv_i2 FROM lv_swap2.
    MODIFY et_state INDEX lv_j2 FROM lv_swap1.
  ENDDO.

ENDFORM.                    "ksa


*&-----------------------------------------------------------------*
*&      Form  prga
*&-----------------------------------------------------------------*
*  Pseudo-Random Generator Algorithm
*------------------------------------------------------------------*
*      -->IT_S       text
*------------------------------------------------------------------*
FORM prga USING it_msg TYPE tty_msg
       CHANGING ht_state TYPE tty_state
                et_out TYPE tty_out.
  DATA:
    lv_out_len TYPE int4,
    lv_state TYPE LINE OF tty_state VALUE 0,
    lv_state_index TYPE int1,
    lv_state_index2 TYPE int2,
    lv_j TYPE int1,
    lv_j2 TYPE int2,
    lv_i TYPE int1,
    lv_i2 TYPE int2,
    lv_swap1 TYPE LINE OF tty_state,
    lv_swap2 TYPE LINE OF tty_state,
    lv_out TYPE LINE OF tty_out.

  DESCRIBE TABLE it_msg LINES lv_out_len.

  DO lv_out_len TIMES.
*    i = (i + 1) % 256;
    lv_i2 = lv_i + 1.
    lv_i = lv_i2 MOD 256.

*    state[i]
    lv_i2 = lv_i + 1.
    READ TABLE ht_state INDEX lv_i2 INTO lv_state.

*    j = (j + state[i]) % 256;
    lv_j2 = lv_j + lv_state.
    lv_j = lv_j2 MOD 256.

*    Swap S[i] and S[j]
    lv_i2 = lv_i + 1.
    lv_j2 = lv_j + 1.
    READ TABLE ht_state INDEX lv_i2 INTO lv_swap1.
    READ TABLE ht_state INDEX lv_j2 INTO lv_swap2.
    MODIFY ht_state INDEX lv_i2 FROM lv_swap2.
    MODIFY ht_state INDEX lv_j2 FROM lv_swap1.

*    out[x] = state[(state[i] + state[j]) % 256];
    lv_state_index2 = lv_swap1 + lv_swap2.
    lv_state_index = lv_state_index2 MOD 256.
    lv_state_index2 = lv_state_index + 1.
    READ TABLE ht_state INDEX lv_state_index2 INTO lv_out.
    APPEND lv_out TO et_out.

  ENDDO.

ENDFORM.                    "prga

*&-------------------------------------------------------------------*
*&      Form  string_to_table
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->IV_STRING  text
*      -->ET_TABLE   text
*--------------------------------------------------------------------*
FORM string_to_table USING iv_string
                  CHANGING et_table TYPE tty_out.

  DATA:
    lv_len TYPE int4,
    lv_i TYPE int1.

  FIELD-SYMBOLS:
    <lv_i> TYPE LINE OF tty_out.


*  lv_len = strlen( iv_string ).
  DESCRIBE FIELD iv_string LENGTH lv_len IN CHARACTER MODE.

  DO lv_len TIMES.
    ASSIGN iv_string+lv_i(1) TO <lv_i> CASTING.
    APPEND <lv_i> TO et_table.
    lv_i = lv_i + 1.
  ENDDO.

ENDFORM.                    "string_to_table


*&-------------------------------------------------------------------*
*&      Form  table_to_string
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
FORM table_to_string  USING it_table TYPE tty_out
                   CHANGING ev_string .

  DATA:
    lv_i TYPE int1,
    lv_char TYPE c.

  FIELD-SYMBOLS:
    <lv_i> TYPE LINE OF tty_out.

  ASSIGN lv_char TO <lv_i> CASTING.
  LOOP AT it_table INTO <lv_i>.

    ev_string+lv_i(1) = lv_char.
    lv_i = lv_i + 1.

  ENDLOOP.

ENDFORM.                    "table_to_string


*&-------------------------------------------------------------------*
*&      Form  encode
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->IT_OUT     text
*      -->HT_MSG     text
*--------------------------------------------------------------------*
FORM encode USING iv_key_char TYPE char128
         CHANGING hv_msg_char TYPE ty_line.

  STATICS:
    lt_state TYPE tty_state,
    lt_key TYPE tty_key.

  DATA:
    lt_msg_o TYPE tty_msg,
    lt_out TYPE tty_out,
    lv_out TYPE LINE OF tty_out,
    lv_msg TYPE LINE OF tty_out,
    lt_msg TYPE tty_msg,
    lv_msg2 TYPE int2.

*  CLEAR et_msg[].

  IF lt_key IS INITIAL.
    PERFORM string_to_table USING iv_key_char CHANGING lt_key.
  ENDIF.
  PERFORM string_to_table USING hv_msg_char CHANGING lt_msg.

  IF lt_state[] IS INITIAL.
    PERFORM ksa USING lt_key lt_msg
             CHANGING lt_state.
  ENDIF.

  PERFORM prga USING lt_msg
            CHANGING lt_state
                     lt_out.
* actual encoding
  LOOP AT lt_msg INTO lv_msg.
    READ TABLE lt_out INDEX sy-tabix INTO lv_out.
    lv_msg2 = lv_msg + lv_out.
    lv_msg = lv_msg2 MOD 256.
    APPEND lv_msg TO lt_msg_o.
  ENDLOOP.

  PERFORM table_to_string USING lt_msg_o CHANGING hv_msg_char.

ENDFORM.                    "encode



*&-------------------------------------------------------------------*
*&      Form  decode
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*      -->IT_OUT     text
*      -->HT_MSG     text
*--------------------------------------------------------------------*
FORM decode USING iv_key TYPE char128
                  it_msg TYPE tty_msg
         CHANGING ev_msg_char TYPE char128.

  DATA:
    lt_state TYPE tty_state,
    lt_out TYPE tty_out,
    lv_out TYPE LINE OF tty_out,
    lv_msg TYPE LINE OF tty_out,
    lt_key TYPE tty_key,
    lt_msg TYPE tty_msg,
    lv_msg2 TYPE int2.

  CLEAR ev_msg_char.

  PERFORM string_to_table USING iv_key CHANGING lt_key.

  PERFORM ksa USING lt_key it_msg
           CHANGING lt_state.

  PERFORM prga USING it_msg
            CHANGING lt_state
                     lt_out.

  LOOP AT it_msg INTO lv_msg.
    READ TABLE lt_out INDEX sy-tabix INTO lv_out.
    lv_msg2 = lv_msg - lv_out.
    lv_msg = lv_msg2 MOD 256.
    APPEND lv_msg TO lt_msg.
  ENDLOOP.

  PERFORM table_to_string USING lt_msg CHANGING ev_msg_char.

ENDFORM.                    "decode

*######################################################################
*######################################################################
*######################################################################
*######################################################################
*######################################################################
*######################################################################

*&--------------------------------------------------------------------*
*&      Form  sos
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM sos.

  FIELD-SYMBOLS:
    <lv_line> TYPE ty_line.

  DATA:
    lt_msg TYPE tty_msg,
    lv_msg_char TYPE char128,
    lt_line TYPE TABLE OF ty_line,
    lt_data_table TYPE tty_line,
    lv_program TYPE progname,
    lv_error TYPE char128,
    lv_line_no  TYPE i.

*  PERFORM get_encrypted_code CHANGING lt_line.

* Get source to be encrypted
  EDITOR-CALL FOR lt_line.

* Encrypt
  LOOP AT lt_line ASSIGNING <lv_line>.
    PERFORM encode USING password
                CHANGING <lv_line>.
  ENDLOOP.

* Prepare encrypted source
  APPEND gv_source_start_tag TO lt_data_table.
  LOOP AT lt_line ASSIGNING <lv_line>.
    PERFORM data_to_table USING <lv_line>
                       CHANGING lt_data_table.
  ENDLOOP.
  APPEND gv_source_end_tag TO lt_data_table.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
     titlebar                    = 'What to do next'
     diagnose_object             = ''
      text_question               = 'Paste below code at the end of Decloack report.'
     text_button_1               = 'Ok'
*     ICON_BUTTON_1               = ' '
*     TEXT_BUTTON_2               = 'Nein'(002)
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
     display_cancel_button       = ''
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
*   IMPORTING
*     ANSWER                      =
*   TABLES
*     PARAMETER                   =
   EXCEPTIONS
     text_not_found              = 1
     OTHERS                      = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* show encrypted code
  EDITOR-CALL FOR lt_data_table.

ENDFORM.                    "sos


*&---------------------------------------------------------------------*
*&      Form  DATA_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LV_LINES>  text
*      <--P_LT_DATA_TABLE  text
*----------------------------------------------------------------------*
FORM data_to_table  USING iv_line TYPE ty_line
                 CHANGING ht_data_table TYPE tty_line.

  DATA:
    lv_data_line TYPE ty_line VALUE '*',
    lv_line_plain TYPE ty_line,
    lv_line_hex TYPE ty_line.

  lv_line_plain = iv_line(28).
  PERFORM decode_for_source USING lv_line_plain lv_line_hex.
  lv_data_line+1 = lv_line_hex.
  APPEND lv_data_line TO ht_data_table.

  lv_line_plain = iv_line+28(28).
  PERFORM decode_for_source USING lv_line_plain lv_line_hex.
  lv_data_line+1 = lv_line_hex.
  APPEND lv_data_line TO ht_data_table.

  lv_line_plain = iv_line+56.
  PERFORM decode_for_source USING lv_line_plain lv_line_hex.
  lv_data_line+1 = lv_line_hex.
  APPEND lv_data_line TO ht_data_table.

ENDFORM.                    " DATA_TO_TABLE


*&---------------------------------------------------------------------*
*&      Form  GET_ENCRYPTED_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_LINE  text
*----------------------------------------------------------------------*
FORM get_encrypted_code CHANGING et_line TYPE tty_line.

  DATA:
    lt_source TYPE tty_line,
    lv_source_index TYPE sytabix.

  FIELD-SYMBOLS:
    <lv_source> TYPE ty_line.

  READ REPORT sy-repid INTO lt_source.

  LOOP AT lt_source ASSIGNING <lv_source>.
    IF <lv_source> = gv_source_start_tag.
      lv_source_index = sy-tabix + 1.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_source ASSIGNING <lv_source> FROM lv_source_index.
    IF <lv_source> = gv_source_end_tag.
      EXIT.
    ENDIF.
    APPEND <lv_source> TO et_line.
  ENDLOOP.

ENDFORM.                    " GET_ENCRYPTED_CODE


*&---------------------------------------------------------------------*
*&      Form  DECODE_FOR_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_LINE1  text
*      -->P_LV_LINE2  text
*----------------------------------------------------------------------*
FORM decode_for_source USING iv_plaintext TYPE ty_line
                    CHANGING ev_hexline TYPE ty_line.

  FIELD-SYMBOLS:
    <lv_hex> TYPE x.

  DATA:
    lv_len TYPE i,
    lv_uc_hexidx TYPE sytabix,
    lv_hexidx TYPE sytabix,
    lv_tmp_hex TYPE char256.

  ASSIGN iv_plaintext TO <lv_hex> CASTING.
  lv_tmp_hex = <lv_hex>.

  lv_len = strlen( iv_plaintext ).

  DO lv_len TIMES.
    ev_hexline+lv_hexidx = lv_tmp_hex+lv_uc_hexidx(2).
    lv_uc_hexidx = lv_uc_hexidx  + 4.
    lv_hexidx = lv_hexidx + 2.
  ENDDO.


ENDFORM.                    " DNCODE_FOR_SOURCE

*&---------------------------------------------------------------------*
*&      Form  ENCODE_FOR_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_LINE1  text
*      -->P_LV_LINE2  text
*----------------------------------------------------------------------*
FORM encode_for_source USING iv_plaintext TYPE ty_line
                    CHANGING ev_hexline TYPE ty_line.

  FIELD-SYMBOLS:
    <lv_hex> TYPE x.

  ASSIGN iv_plaintext TO <lv_hex> CASTING.
  ev_hexline = <lv_hex>.


ENDFORM.                    " ENCODE_FOR_SOURCE