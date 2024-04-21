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

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    " Normalize and sort the input string
    lv_input_sorted = sort_string( to_lower( input ) ).

    " Loop through each candidate and check if it is an anagram of the input
    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_input_sorted = sort_string( to_lower( lv_candidate ) ) AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  " Helper method to sort a string by its characters
  METHOD sort_string.
    DATA: lt_chars TYPE TABLE OF char,
          lv_sorted_string TYPE string.

    " Convert string to a table of characters
    lt_chars = to_table( string ).
    " Sort the characters
    SORT lt_chars.
    " Convert the sorted table of characters back to a string
    CONCATENATE LINES OF lt_chars INTO lv_sorted_string.
    string = lv_sorted_string.
  ENDMETHOD.

ENDCLASS.
