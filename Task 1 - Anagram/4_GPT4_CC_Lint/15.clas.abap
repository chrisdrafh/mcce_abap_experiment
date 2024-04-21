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
    METHODS normalize_string
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input TYPE string.
    lv_input = normalize_string( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_input = normalize_string( lv_candidate ) AND input <> lv_candidate.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    rv_string = to_upper( iv_string ). " Convert to uppercase for case-insensitive comparison
    SORT rv_string BY ( val = val ASCENDING ). " Sort characters alphabetically
  ENDMETHOD.
ENDCLASS.
