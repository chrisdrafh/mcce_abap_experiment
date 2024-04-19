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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table.

    " Use regular expressions to split the input string on non-word characters except apostrophes
    " within words, considering numbers as words and normalize to lower case.
    SPLIT phrase AT `\([^a-zA-Z0-9']\|\s+\)` INTO TABLE lt_words.

    " Remove empty entries that may result from consecutive non-word characters
    DELETE lt_words WHERE table_line IS INITIAL.

    " Convert words to lower case and count them
    LOOP AT lt_words INTO lv_word.
      " Normalize to lower case
      lv_word = to_lower( lv_word ).

      " Check if the word is already in the result table
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Increment count if the word exists
        MODIFY lt_result FROM VALUE #( word = lv_word count = lt_result[ word = lv_word ]-count + 1 ) TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " Add new word with count 1
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
