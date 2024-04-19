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
    METHODS is_anagram
      IMPORTING
        word1         TYPE string
        word2         TYPE string
      RETURNING
        VALUE(is_ana) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( input, candidate ) = abap_true AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    " Normalize and sort the characters of both words
    DATA(norm_word1) = to_lower( word1 ).
    DATA(norm_word2) = to_lower( word2 ).

    SORT norm_word1 BY character.
    SORT norm_word2 BY character.

    " Check if sorted strings are identical
    is_ana = xsdbool( norm_word1 = norm_word2 ).
  ENDMETHOD.

ENDCLASS.
