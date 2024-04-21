CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS normalize_string
      IMPORTING
        VALUE(iv_string) TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    lv_normalized_input = normalize_string( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      lv_normalized_candidate = normalize_string( lv_candidate ).
      
      " Check if normalized candidate matches the normalized input and is not the same as input
      IF lv_normalized_candidate = lv_normalized_input AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    " Convert to lower case and sort the characters of the string
    DATA(lv_chars) TYPE SORTED TABLE OF char WITH UNIQUE DEFAULT KEY.
    DATA(lv_char)  TYPE char.

    " Break string into characters, normalize case, sort, and reassemble
    LOOP AT cl_abap_conv_in_ce=>uccpi( iv_string ) INTO lv_char.
      INSERT lv_char TO lv_chars ASSIGNING FIELD-SYMBOL(<fs_char>).
    ENDLOOP.

    SORT lv_chars.

    CLEAR rv_string.
    LOOP AT lv_chars INTO lv_char.
      CONCATENATE rv_string lv_char INTO rv_string.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
