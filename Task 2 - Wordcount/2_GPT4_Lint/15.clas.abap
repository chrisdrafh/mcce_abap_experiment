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
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table,
          ls_result_line LIKE LINE OF lt_result.

    " Replace punctuation with spaces, but preserve apostrophes in contractions
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN phrase WITH ' '.
    " Split words based on whitespace
    SPLIT phrase AT space INTO TABLE lt_words.

    " Normalize case and count words
    LOOP AT lt_words INTO lv_word.
      " Convert to lowercase for case insensitivity
      lv_word = to_lower( lv_word ).
      " Check if the word already exists in the result table
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result_line.
      IF sy-subrc = 0.
        " Increase count if word exists
        ls_result_line-count = ls_result_line-count + 1.
        MODIFY lt_result FROM ls_result_line.
      ELSE.
        " Add new word with count 1
        ls_result_line-word = lv_word.
        ls_result_line-count = 1.
        APPEND ls_result_line TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
