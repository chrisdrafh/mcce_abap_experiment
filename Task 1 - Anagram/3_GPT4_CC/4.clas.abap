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
        word TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string.

    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalized_input = normalize_word( candidate ) AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert the word to uppercase and sort the characters alphabetically
    DATA(lv_word) = word.
    TRANSLATE lv_word TO UPPER CASE.
    SORT lv_word AS TEXT.
    normalized_word = lv_word.
  ENDMETHOD.
ENDCLASS.
