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
    METHODS normalize_and_sort
      IMPORTING
        iv_word TYPE string
      RETURNING
        VALUE(rv_sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_sorted_input TYPE string,
          lv_sorted_candidate TYPE string.

    " Normalize and sort the target input
    lv_sorted_input = normalize_and_sort( input ).

    " Check each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate =@ input.
        CONTINUE. " Skip if candidate is exactly the input
      ENDIF.

      " Normalize and sort the candidate
      lv_sorted_candidate = normalize_and_sort( lv_candidate ).

      " Check if sorted candidate matches sorted input
      IF lv_sorted_candidate = lv_sorted_input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_and_sort.
    " Convert to uppercase and sort characters
    rv_sorted = to_upper( iv_word ).
    SORT rv_sorted BY ( val = rv_sorted ).
  ENDMETHOD.
ENDCLASS.
