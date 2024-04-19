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
          lv_word  TYPE string,
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Replace all punctuation except apostrophe (') with space and split the string
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN phrase WITH ` ` INTO phrase.
    SPLIT phrase AT ` ` INTO TABLE lt_words.

    " Convert words to lower case and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Word already exists, increment count
          READ TABLE lt_result INTO ls_result WITH KEY word = lv_word.
          ls_result-count = ls_result-count + 1.
          MODIFY lt_result FROM ls_result TRANSPORTING count WHERE word = ls_result-word.
        ELSE.
          " New word, add to result table
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
