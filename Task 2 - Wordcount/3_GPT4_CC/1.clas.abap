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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA: lt_results TYPE return_table.
    DATA: lv_word TYPE string.
    DATA: ls_result LIKE LINE OF lt_results.
    
    " Normalize input to handle case insensitivity
    phrase = to_lower( phrase ).

    " Split input into words based on punctuation and whitespace, except apostrophes
    SPLIT phrase AT '[\s,.:;!?"]+' INTO TABLE lt_words.

    " Remove empty strings which might have been added due to multiple punctuations
    DELETE lt_words WHERE table_line IS INITIAL.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_results WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " Word not yet in results, add it
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO lt_results.
      ELSE.
        " Increment count for existing word
        READ TABLE lt_results INTO ls_result WITH KEY word = lv_word.
        ls_result-count += 1.
        MODIFY lt_results FROM ls_result INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    result = lt_results.
  ENDMETHOD.

ENDCLASS.
