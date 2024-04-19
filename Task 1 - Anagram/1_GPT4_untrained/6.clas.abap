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
    DATA: norm_input TYPE string.
    norm_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF norm_input = normalize_word( candidate ) AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert the word to lower case and sort the characters
    DATA: char_table TYPE SORTED TABLE OF char.
    " Convert to lowercase
    word = to_lower( word ).
    " Split the word into characters and sort them
    DO strlen( word ) TIMES.
      INSERT word+sy-index(1) INTO TABLE char_table.
    ENDDO.
    SORT char_table.
    " Concatenate sorted characters back to a string
    CLEAR norm_word.
    LOOP AT char_table INTO DATA(char).
      CONCATENATE norm_word char INTO norm_word.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
