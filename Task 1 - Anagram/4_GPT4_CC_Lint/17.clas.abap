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
    METHODS normalize
      IMPORTING
        VALUE(word) TYPE string
      RETURNING
        VALUE(normalized) TYPE string.

    METHODS is_anagram
      IMPORTING
        VALUE(word1) TYPE string
        VALUE(word2) TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = normalize( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( normalize( candidate ), normalized_input ) AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize.
    normalized = to_lower( word ).
  ENDMETHOD.

  METHOD is_anagram.
    DATA: sorted_word1 TYPE string,
          sorted_word2 TYPE string.

    sorted_word1 = word1.
    sorted_word2 = word2.

    SORT sorted_word1.
    SORT sorted_word2.

    is_anagram = xsdbool( sorted_word1 = sorted_word2 ).
  ENDMETHOD.

ENDCLASS.
