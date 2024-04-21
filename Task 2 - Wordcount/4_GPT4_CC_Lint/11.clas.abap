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
          ls_result LIKE LINE OF result.

    " Normalize the phrase: convert to lowercase and replace non-apostrophe punctuation with spaces
    phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN phrase WITH ' '.

    " Split the phrase into words based on whitespace
    SPLIT phrase AT space INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Increment count if word exists
          READ TABLE result INTO ls_result WITH KEY word = lv_word.
          ls_result-count = ls_result-count + 1.
          MODIFY result FROM ls_result INDEX sy-tabix.
        ELSE.
          " Add new word with count 1
          ls_result-word = lv_word.
          ls_result-count = 1.
          APPEND ls_result TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
