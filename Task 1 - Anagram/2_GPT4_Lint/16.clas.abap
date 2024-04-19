CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS sort_string
      IMPORTING
        VALUE(iv_str) TYPE string
      RETURNING
        VALUE(rv_str) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.
    lv_input_sorted = sort_string( iv_str = input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND sort_string( lv_candidate ) = lv_input_sorted.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    DATA: lt_chars TYPE TABLE OF char,
          lv_char  TYPE char.

    " Convert string to a table of characters and sort
    DO strlen( iv_str ) TIMES.
      lv_char = to_lower( substring( val = iv_str off = sy-index - 1 len = 1 ) ).
      APPEND lv_char TO lt_chars.
    ENDDO.
    SORT lt_chars.
    " Convert the sorted table of characters back to a string
    LOOP AT lt_chars INTO lv_char.
      CONCATENATE rv_str lv_char INTO rv_str.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
