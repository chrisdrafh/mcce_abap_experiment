CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
    
    " Normalize phrase: convert to lower case
    phrase = to_lower( phrase ).

    " Replace all punctuations (except apostrophe in contractions) with space
    REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9\']' IN phrase WITH space.

    " Split the normalized string into words
    SPLIT phrase AT space INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      " Ignore empty strings resulting from multiple spaces
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          " Add new word to result table
          ls_result-word = lv_word.
          ls_result-count = 1.
          APPEND ls_result TO result.
        ELSE.
          " Increment count of existing word
          READ TABLE result INTO ls_result WITH KEY word = lv_word.
          ls_result-count = ls_result-count + 1.
          MODIFY result FROM ls_result INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
