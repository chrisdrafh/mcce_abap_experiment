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
        word      TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA(normalized_input) TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    normalized_word = to_lower( word ).
    SORT normalized_word BY ( ASCENDING ).
  ENDMETHOD.
ENDCLASS.
