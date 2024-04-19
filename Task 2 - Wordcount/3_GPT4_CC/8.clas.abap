CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
        VALUE(result) TYPE return_table .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS split_into_words
      IMPORTING
        !phrase     TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          ls_word_count TYPE return_structure.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO DATA(lv_word).
      lv_word = to_lower( lv_word ). " Normalize to lowercase
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word exists, increment count
        MODIFY result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 )
          TRANSPORTING count
          WHERE word = lv_word.
      ELSE.
        " New word, initialize count
        ls_word_count-word = lv_word.
        ls_word_count-count = 1.
        INSERT ls_word_count INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_into_words.
    " Splits the phrase into words based on non-word characters except for apostrophes
    " for contractions, and returns a table of words.
    SPLIT phrase AT '[^[:alnum:]'']+' INTO TABLE words.
  ENDMETHOD.

ENDCLASS.
