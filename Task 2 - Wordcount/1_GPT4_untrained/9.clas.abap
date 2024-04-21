CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
             word  TYPE string,
             count TYPE i,
           END OF return_structure,
           return_table TYPE SORTED TABLE OF return_structure WITH UNIQUE KEY word.

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
          lv_word  TYPE string.

    " Normalize input to handle case insensitivity
    phrase = to_lower( phrase ).

    " Replace all punctuation (except apostrophes) with space
    REPLACE ALL OCCURRENCES OF REGEX '[[:punct:]&&[^'']]' IN phrase WITH ' '.

    " Split the phrase into words based on space
    SPLIT phrase AT space INTO TABLE lt_words.

    " Prepare result table
    LOOP AT lt_words INTO lv_word.
      " Check if the word is not just an empty string after trim (might happen with multiple spaces)
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Increment count if word exists
          MODIFY TABLE result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 ).
        ELSE.
          " Add new word with count 1
          APPEND VALUE #( word = lv_word count = 1 ) TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
