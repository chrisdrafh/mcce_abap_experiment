CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF return_structure,
        word  TYPE string,
        count TYPE i,
      END OF return_structure,
      return_table TYPE STANDARD TABLE OF return_structure WITH KEY word.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Normalize input to lowercase
    phrase = to_lower( phrase ).

    " Tokenize the input based on specified punctuation and whitespace
    " Replace non-word and non-apostrophe characters with space
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN phrase WITH space.

    " Split phrase into words based on spaces
    SPLIT phrase AT space INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      " Check if the word is already in the result table
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
      IF sy-subrc = 0.
        " Increment count if word exists
        ls_result-count = ls_result-count + 1.
        MODIFY lt_result FROM ls_result TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " Add new word with count 1
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    " Return the result
    result = lt_result.
  ENDMETHOD.

ENDCLASS.
