CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
             word  TYPE string,
             count TYPE i,
           END OF return_structure,
           return_table TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY.

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
          ls_result_line LIKE LINE OF lt_result,
          lv_clean_phrase TYPE string.

    " Normalize case and replace punctuations with space, except for apostrophes in contractions
    lv_clean_phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN lv_clean_phrase WITH ' '.

    " Split the cleaned phrase into words
    SPLIT lv_clean_phrase AT space INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word exists, increase count
        READ TABLE lt_result INTO ls_result_line INDEX sy-tabix.
        ls_result_line-count = ls_result_line-count + 1.
        MODIFY TABLE lt_result FROM ls_result_line INDEX sy-tabix.
      ELSE.
        " Otherwise, add new word with count 1
        ls_result_line-word = lv_word.
        ls_result_line-count = 1.
        APPEND ls_result_line TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
