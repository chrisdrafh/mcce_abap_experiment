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
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_word.
    " Normalize the word by converting to lower case and sorting the characters
    DATA: char_table TYPE TABLE OF char.

    " Convert to lowercase for case insensitive comparison
    word = to_lower( word ).

    " Convert string to character table, sort it and convert back to string
    DO strlen( word ) TIMES.
      APPEND word+sy-index(1) TO char_table.
    ENDDO.
    SORT char_table.
    CLEAR normalized_word.
    LOOP AT char_table INTO DATA(char).
      CONCATENATE normalized_word char INTO normalized_word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
