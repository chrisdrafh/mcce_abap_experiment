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
        iv_word       TYPE string
      RETURNING
        VALUE(rv_sorted_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_sorted_input TYPE string,
          lt_result       TYPE string_table.

    lv_sorted_input = normalize_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_sort( lv_candidate ) = lv_sorted_input AND lv_candidate <> input.
        APPEND lv_candidate TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_sort.
    DATA: lt_chars TYPE TABLE OF char.

    " Convert the input to upper case to make the comparison case insensitive
    " and split into individual characters
    lt_chars = cl_abap_char_utilities=>to_upper( iv_word ) TO_TABLE.

    " Sort the characters
    SORT lt_chars.

    " Concatenate the sorted characters back into a string
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE rv_sorted_word lv_char INTO rv_sorted_word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
