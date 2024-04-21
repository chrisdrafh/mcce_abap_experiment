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
          ls_result LIKE LINE OF lt_result,
          lv_clean_phrase TYPE string.

    " Remove all punctuation except apostrophes, convert to lower case
    lv_clean_phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN lv_clean_phrase WITH space.

    " Split the cleaned-up phrase into words
    SPLIT lv_clean_phrase AT space INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ELSE.
        READ TABLE lt_result INTO ls_result WITH KEY word = lv_word.
        ls_result-count = ls_result-count + 1.
        MODIFY TABLE lt_result FROM ls_result TRANSPORTING count WHERE word = lv_word.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
