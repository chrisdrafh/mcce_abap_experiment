CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: tt_string_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE tt_string_table
      RETURNING
        VALUE(result) TYPE tt_string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS sort_string
      IMPORTING
        iv_string     TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    " Normalize the input word
    DATA: lv_sorted_input TYPE string.
    lv_sorted_input = sort_string( iv_string = input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Check if the candidate is not the same as the input and is an anagram
      IF lv_candidate <> input AND
         sort_string( iv_string = lv_candidate ) = lv_sorted_input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    " Convert to lower case and sort the characters of the string
    DATA: lt_chars TYPE tt_string_table.
    SPLIT iv_string TO LOWER( iv_string ) AT '' INTO TABLE lt_chars.
    SORT lt_chars.
    CONDENSE lt_chars INTO rv_string SEPARATED BY ''.
  ENDMETHOD.
ENDCLASS.
