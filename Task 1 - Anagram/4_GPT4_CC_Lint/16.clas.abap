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
    METHODS normalize_sort
      IMPORTING
        iv_word TYPE string
      RETURNING
        VALUE(rv_sorted) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    lv_input_sorted = normalize_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_sort( lv_candidate ) = lv_input_sorted AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_sort.
    " Normalize by converting to lower case and sort the characters
    DATA: lv_chars TYPE string.

    " Convert to lower case
    lv_chars = to_lower( iv_word ).
    " Convert string to character table, sort it, and convert back to string
    SORT lv_chars AS TEXT.
    rv_sorted = lv_chars.

  ENDMETHOD.

ENDCLASS.
