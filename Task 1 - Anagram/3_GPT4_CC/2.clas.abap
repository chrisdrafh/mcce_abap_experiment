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
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    result = VALUE #( ).

    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( input, candidate ) = abap_true AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA(sorted_word1) = to_upper( CONV string( word1 ) ).
    DATA(sorted_word2) = to_upper( CONV string( word2 ) ).

    " Sort the characters of both words to check if they are anagrams
    SORT CHARACTERS OF sorted_word1.
    SORT CHARACTERS OF sorted_word2.

    is_anagram = xsdbool( sorted_word1 = sorted_word2 ).
  ENDMETHOD.

ENDCLASS.
