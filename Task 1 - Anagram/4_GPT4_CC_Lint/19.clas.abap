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
    METHODS sort_characters
      IMPORTING
        word       TYPE string
      RETURNING
        VALUE(sorted_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = sort_characters( to_lower( input ) ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalized_input = sort_characters( to_lower( candidate ) )
         AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_characters.
    DATA: lt_chars TYPE TABLE OF char.
    SPLIT word AT '' INTO TABLE lt_chars.
    SORT lt_chars.
    CONCATENATE LINES OF lt_chars INTO sorted_word.
  ENDMETHOD.

ENDCLASS.
