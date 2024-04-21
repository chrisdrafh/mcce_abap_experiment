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
    METHODS normalize_word
      IMPORTING
        iv_word       TYPE string
      RETURNING
        VALUE(rv_normalized) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalized_input = normalize_word( candidate ) AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    DATA: lt_chars TYPE TABLE OF char.

    " Convert word to lower case and split into characters
    lv_normalized = to_lower( iv_word ).
    lt_chars[] = cl_abap_conv_in_ce=>string_to_table( lv_normalized ).

    " Sort characters
    SORT lt_chars.

    " Join characters back to string
    rv_normalized = cl_abap_conv_in_ce=>table_to_string( lt_chars ).
  ENDMETHOD.
ENDCLASS.
