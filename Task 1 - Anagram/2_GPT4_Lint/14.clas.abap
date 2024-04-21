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

  PRIVATE SECTION.
    METHODS sort_string
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(rv_sorted_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    " Sort the input string for comparison
    lv_input_sorted = sort_string( to_lower( input ) ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      DATA(lv_candidate_sorted) = sort_string( to_lower( lv_candidate ) ).
      
      " Compare sorted candidate with sorted input, exclude the same word
      IF lv_candidate_sorted = lv_input_sorted AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    " Convert the string to a character table, sort it, and convert it back
    DATA: lt_chars TYPE TABLE OF char.

    lt_chars = cl_abap_char_utilities=>string_to_table( iv_string ).
    SORT lt_chars.
    rv_sorted_string = cl_abap_char_utilities=>table_to_string( lt_chars ).
  ENDMETHOD.
ENDCLASS.
