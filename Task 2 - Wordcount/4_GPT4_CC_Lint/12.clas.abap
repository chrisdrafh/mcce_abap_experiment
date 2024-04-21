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
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Use regular expressions to split the input while considering contractions
    SPLIT phrase AT '[^a-zA-Z0-9\']+' INTO TABLE lt_words.

    " Process each word, convert to lowercase and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ). " Convert word to lowercase

      " Check if the word already exists in the result table
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
      IF sy-subrc = 0.
        " Increment count if word exists
        ls_result-count += 1.
        MODIFY lt_result FROM ls_result.
      ELSE.
        " Add new word with count 1 if not exists
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO lt_result.
      ENDIF.
    ENDLOOP.

    " Return the result table
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
