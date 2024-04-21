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
          lv_word  TYPE string.

    " Remove punctuations, except apostrophe in contractions
    REPLACE ALL OCCURRENCES OF REGEX '[^\w''-]' IN phrase WITH space.
    
    " Split the string into words based on whitespace
    SPLIT phrase AT space INTO TABLE lt_words.

    " Prepare to count words
    DATA: lt_word_count TYPE return_table.

    " Loop through each word
    LOOP AT lt_words INTO lv_word.
      " Convert to lower case to handle case insensitivity
      lv_word = to_lower( lv_word ).

      " Check if the word is already in the table
      READ TABLE lt_word_count WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word found, increment count
        MODIFY TABLE lt_word_count FROM VALUE #( word = lv_word count = lt_word_count[ word = lv_word ]-count + 1 ).
      ELSE.
        " New word, add to table
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_word_count.
      ENDIF.
    ENDLOOP.

    " Return the results
    result = lt_word_count.
  ENDMETHOD.

ENDCLASS.
