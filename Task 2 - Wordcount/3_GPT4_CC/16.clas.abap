CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
        VALUE(result) TYPE return_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Normalize the case
    phrase = to_lower( phrase ).

    " Replace all punctuation except apostrophes and alphanumeric with space
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN phrase WITH space.
    
    " Split the phrase into words
    SPLIT phrase AT space INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      " Check if the word exists in the result table
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
      IF sy-subrc = 0.
        " Increment count if word already exists
        ls_result-count += 1.
        MODIFY lt_result FROM ls_result.
      ELSE.
        " Add new word to the table with a count of 1
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
