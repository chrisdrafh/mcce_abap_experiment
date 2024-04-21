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
    METHODS standardize_and_sort
      IMPORTING
        iv_word       TYPE string
      RETURNING
        VALUE(rv_sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.
    lv_input_sorted = standardize_and_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      DATA(lv_candidate_sorted) = standardize_and_sort( lv_candidate ).

      IF lv_candidate_sorted = lv_input_sorted AND NOT compare_text( lv_candidate, input ).
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD standardize_and_sort.
    DATA: lt_chars TYPE TABLE OF char.
    " Convert to lower case and split into characters
    SPLIT to_lower( iv_word ) AT '' INTO TABLE lt_chars.
    " Sort the characters
    SORT lt_chars.
    " Concatenate back to a string
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE rv_sorted lv_char INTO rv_sorted.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" Function to compare text irrespective of case
FUNCTION compare_text.
  EXPORTING
    iv_text1      TYPE string
    iv_text2      TYPE string
  RETURNING
    VALUE(rv_same) TYPE abap_bool.

  rv_same = xsdbool( to_upper( iv_text1 ) = to_upper( iv_text2 ) ).
ENDFUNCTION.
