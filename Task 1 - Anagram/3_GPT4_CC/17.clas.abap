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
    METHODS convert_to_sorted_lower_case
      IMPORTING
        iv_word        TYPE string
      RETURNING
        VALUE(rv_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    lv_input_sorted = convert_to_sorted_lower_case( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_input_sorted = convert_to_sorted_lower_case( lv_candidate ) AND
         lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_to_sorted_lower_case.
    rv_word = to_lower( iv_word ).
    SORT rv_word AS TEXT.
  ENDMETHOD.
ENDCLASS.
