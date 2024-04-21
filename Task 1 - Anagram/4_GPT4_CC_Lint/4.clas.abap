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
        iv_string      TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA(normalized_input) = normalize_string( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_string( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    rv_string = to_lower( iv_string ).
    SORT rv_string BY (substring = rv_string).
  ENDMETHOD.
ENDCLASS.
