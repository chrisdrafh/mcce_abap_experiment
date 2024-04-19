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
        iv_string     TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    " Normalize the input for comparison
    lv_normalized_input = normalize_string( input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Normalize candidate for comparison
      lv_normalized_candidate = normalize_string( lv_candidate ).

      " Check for anagram condition, excluding the exact match to input
      IF lv_normalized_candidate = lv_normalized_input AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    " Normalize string by converting to upper case and sorting the characters
    DATA: lt_chars TYPE TABLE OF c LENGTH 1.

    " Convert string to upper case and split into table of characters
    lt_chars = cl_abap_char_utilities=>horizontal_to_vertical( to_upper( iv_string ) ).
    " Sort characters
    SORT lt_chars.
    " Convert sorted table of characters back to string
    rv_string = cl_abap_char_utilities=>vertical_to_horizontal( lt_chars ).
  ENDMETHOD.

ENDCLASS.
