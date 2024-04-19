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
    METHODS normalize_and_sort
      IMPORTING
        VALUE(iv_word) TYPE string
      RETURNING
        VALUE(rv_sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string.
    lv_normalized_input = normalize_and_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND normalize_and_sort( lv_candidate ) = lv_normalized_input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_and_sort.
    DATA: lv_chars TYPE string_table.
    SPLIT iv_word AT '' INTO TABLE lv_chars.
    SORT lv_chars AS TEXT IGNORING CASE.
    CONCATENATE LINES OF lv_chars INTO rv_sorted.
  ENDMETHOD.
ENDCLASS.
