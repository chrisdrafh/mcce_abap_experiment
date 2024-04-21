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
    METHODS sanitize_input
      IMPORTING
        !input    TYPE string
      RETURNING
        VALUE(cleaned) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lv_cleaned_input TYPE string.

    " Sanitize input to remove special characters except apostrophes and convert to lowercase
    lv_cleaned_input = sanitize_input( phrase ).

    " Split the sanitized input into words
    SPLIT lv_cleaned_input AT '\s+' INTO TABLE lt_words.

    " Initialize the result table
    CLEAR result.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word exists in result, increase count
        MODIFY result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 )
                      TRANSPORTING count
                      WHERE word = lv_word.
      ELSE.
        " Add new word with count 1
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD sanitize_input.
    " Replace all punctuation except apostrophes with space and convert to lowercase
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN input WITH ' ' INTO cleaned.
    cleaned = to_lower( cleaned ).
  ENDMETHOD.

ENDCLASS.
