CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS: normalize_word
      IMPORTING
        word          TYPE string
      RETURNING
        VALUE(sorted_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA(normalized_input) = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    DATA(lv_word) = word.
    REPLACE ALL OCCURRENCES OF space IN lv_word WITH ''.
    TRANSLATE lv_word TO UPPER CASE.
    SORT lv_word BY ( ASCENDING COMPONENTS ).
    CONCATENATE LINES OF lv_word INTO sorted_word SEPARATED BY ''.
  ENDMETHOD.
ENDCLASS.
