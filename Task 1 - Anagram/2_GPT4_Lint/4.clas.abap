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
    METHODS to_sorted_string
      IMPORTING
        iv_string     TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_sorted_input TYPE string.
    lv_sorted_input = to_sorted_string( iv_string = input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_sorted_input = to_sorted_string( iv_string = lv_candidate ) AND
         NOT ( CONDENSE( lv_candidate ) = CONDENSE( input ) ).
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD to_sorted_string.
    DATA: lt_chars TYPE TABLE OF char.
    SPLIT iv_string AT '' INTO TABLE lt_chars.
    SORT lt_chars AS TEXT.
    CONCATENATE LINES OF lt_chars INTO rv_string.
    TRANSLATE rv_string TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
