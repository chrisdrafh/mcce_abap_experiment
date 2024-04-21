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
          ls_result TYPE return_structure.

    " Normalize input to lower case
    phrase = to_lower( phrase ).

    " Regular expression to match words and numbers, ignoring contractions
    FIND ALL OCCURRENCES OF REGEX '(\b\w+('\w+)?\b)' IN phrase MATCHING SUBSTRINGS INTO lt_words.

    " Initialize result table
    CLEAR result.

    " Count occurrences
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word exists in table, increase count
        READ TABLE result INTO ls_result WITH KEY word = lv_word.
        ls_result-count = ls_result-count + 1.
        MODIFY result FROM ls_result INDEX sy-tabix.
      ELSE.
        " New word, add to table
        ls_result-word  = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
