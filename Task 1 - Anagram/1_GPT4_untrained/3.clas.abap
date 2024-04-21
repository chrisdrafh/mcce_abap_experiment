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
    METHODS normalize_and_sort
      IMPORTING
        word        TYPE string
      RETURNING
        VALUE(sorted_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string,
          normalized_candidate TYPE string.

    " Normalize and sort the input word
    normalized_input = normalize_and_sort( input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(candidate).
      " Normalize and sort candidate
      normalized_candidate = normalize_and_sort( candidate ).

      " Check if sorted strings match and the candidate is not the original input
      IF normalized_candidate = normalized_input AND candidate <> input.
        " Add to result if conditions are met
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_and_sort.
    " Convert to lower case and split into a table of single characters
    DATA(char_table) = cl_abap_conv_in_ce=>uccpi( word ).

    " Sort the characters alphabetically
    SORT char_table.

    " Convert back to a string
    LOOP AT char_table INTO DATA(char).
      sorted_word = sorted_word && char.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
