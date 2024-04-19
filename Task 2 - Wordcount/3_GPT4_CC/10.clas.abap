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
      return_table TYPE SORTED TABLE OF return_structure WITH UNIQUE KEY word.
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
          ls_count TYPE return_structure,
          lv_word TYPE string.

    " Normalize input to lowercase
    phrase = to_lower( phrase ).

    " Use regular expression to split the input into words
    FIND ALL OCCURRENCES OF REGEX '[a-z0-9]+' IN phrase MATCHING SUBSTRINGS INTO lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word already in the table, increment the count
        READ TABLE result INTO ls_count WITH KEY word = lv_word.
        ls_count-count = ls_count-count + 1.
        MODIFY result FROM ls_count TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " New word, add to the table
        ls_count-word = lv_word.
        ls_count-count = 1.
        INSERT ls_count INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
