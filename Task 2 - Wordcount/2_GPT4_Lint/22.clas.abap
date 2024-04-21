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
          lv_clean_phrase TYPE string,
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Normalize the input phrase to lower case
    lv_clean_phrase = to_lower( phrase ).

    " Replace all punctuation except apostrophes with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN lv_clean_phrase WITH ' '.

    " Split the cleaned phrase into words based on whitespace
    SPLIT lv_clean_phrase AT space INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word exists, increment the count
        MODIFY TABLE lt_result FROM VALUE #( word = lv_word count = ls_result.count + 1 ) TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " New word, add to the table
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO lt_result.
      ENDIF.
    ENDLOOP.

    " Return the table of results
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
