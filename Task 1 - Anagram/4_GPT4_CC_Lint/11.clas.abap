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

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = to_lower( input ).

    " Sort the characters of the input word
    DATA(sorted_input) = zcl_anagram=>sort_characters( normalized_input ).

    " Loop through the candidates and check for anagrams
    LOOP AT candidates INTO DATA(candidate).
      DATA: normalized_candidate TYPE string.
      normalized_candidate = to_lower( candidate ).

      " Avoid comparing the candidate with itself
      IF normalized_candidate = normalized_input.
        CONTINUE.
      ENDIF.

      " Sort and compare
      IF zcl_anagram=>sort_characters( normalized_candidate ) = sorted_input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  " Helper static method to sort characters of a word
  METHOD sort_characters.
    " Convert the string to a table of characters
    DATA: character_table TYPE TABLE OF char.
    DO strlen( input ) TIMES.
      APPEND substring( val = input off = sy-index - 1 len = 1 ) TO character_table.
    ENDDO.

    " Sort the table
    SORT character_table.

    " Convert the table back to a string
    CLEAR input.
    LOOP AT character_table INTO DATA(character).
      CONCATENATE input character INTO input.
    ENDLOOP.
    result = input.
  ENDMETHOD.

ENDCLASS.
