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
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anag) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    " Initialize result table
    CLEAR result.

    " Iterate over candidate words
    LOOP AT candidates INTO DATA(candidate).
      " Check if the candidate is an anagram of the input and not identical
      IF is_anagram( input, candidate ) = abap_true AND
         NOT input = candidate.
        " Append to result if it's a valid anagram
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    " Normalize both words by converting to lower case and sorting the characters
    DATA(lv_word1_sorted) = CONDENSE( to_lower( word1 ) ).
    DATA(lv_word2_sorted) = CONDENSE( to_lower( word2 ) ).

    " Sort the characters in the string
    lv_word1_sorted = sort_string( lv_word1_sorted ).
    lv_word2_sorted = sort_string( lv_word2_sorted ).

    " Compare the sorted strings to determine if they are anagrams
    is_anag = xsdbool( lv_word1_sorted = lv_word2_sorted ).
  ENDMETHOD.

  METHOD sort_string.
    " Helper method to sort characters in a string
    DATA: lt_chars TYPE TABLE OF char,
          lv_char  TYPE char.

    " Split the string into a table of characters
    DO strlen( input ) TIMES.
      lv_char = substring( val = input off = sy-index len = 1 ).
      APPEND lv_char TO lt_chars.
    ENDDO.

    " Sort the table
    SORT lt_chars.

    " Concatenate back to string
    CLEAR input.
    LOOP AT lt_chars INTO lv_char.
      CONCATENATE input lv_char INTO input.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
