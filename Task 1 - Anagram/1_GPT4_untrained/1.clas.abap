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
    METHODS normalize_word
      IMPORTING
        word          TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.

    DATA normalized_input TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      DATA(normalized_candidate) = normalize_word( candidate ).

      IF normalized_input = normalized_candidate AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_word.

    normalized_word = word.
    TRANSLATE normalized_word TO LOWER CASE.
    SORT STRING normalized_word AS TEXT.

  ENDMETHOD.

ENDCLASS.
