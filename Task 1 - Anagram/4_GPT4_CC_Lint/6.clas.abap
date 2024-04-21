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
    METHODS normalize_word
      IMPORTING
        iv_word TYPE string
      RETURNING
        VALUE(rv_normalized) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = normalize_word( iv_word = input ).

    LOOP AT candidates INTO DATA(candidate).
      DATA: normalized_candidate TYPE string.
      normalized_candidate = normalize_word( iv_word = candidate ).
      IF normalized_candidate = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    rv_normalized = to_upper( iv_word ).
    SORT rv_normalized BY (val = rv_normalized).
  ENDMETHOD.

ENDCLASS.
