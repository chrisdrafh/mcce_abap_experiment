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
        word       TYPE string
      RETURNING
        VALUE(norm_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    " Normalize the input word for comparison
    DATA: normalized_input TYPE string.
    normalized_input = normalize_word( input ).

    " Loop through each candidate word
    LOOP AT candidates INTO DATA(candidate).
      " Normalize candidate and compare
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert the word to lowercase and sort the characters
    norm_word = to_lower( word ).
    DATA(sorted_characters) TYPE string_table.
    " Split the word into individual characters
    DO strlen( norm_word ) TIMES.
      APPEND norm_word+sy-index(1) TO sorted_characters.
    ENDDO.
    " Sort the characters alphabetically
    SORT sorted_characters.
    CLEAR norm_word.
    " Concatenate the sorted characters back to a string
    LOOP AT sorted_characters INTO DATA(character).
      CONCATENATE norm_word character INTO norm_word.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
