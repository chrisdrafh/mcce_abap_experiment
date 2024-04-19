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
    DATA: normalized_input TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert to lowercase and sort the characters in the word
    DATA(lv_word) = to_lower( word ).
    DATA: lt_chars TYPE TABLE OF char.
    DO strlen( lv_word ) TIMES.
      APPEND lv_word+sy-index(1) TO lt_chars.
    ENDDO.
    SORT lt_chars.
    CLEAR normalized_word.
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE normalized_word lv_char INTO normalized_word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
