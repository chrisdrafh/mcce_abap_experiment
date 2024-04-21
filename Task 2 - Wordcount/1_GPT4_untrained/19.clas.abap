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
      return_table TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY.

    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS split_into_words
      IMPORTING
        !text       TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    lt_words = split_into_words( phrase ).

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result ASSIGNING FIELD-SYMBOL(<fs_result>) WITH KEY word = lv_word.
      IF sy-subrc = 0.
        " Increase count if word already exists
        <fs_result>-count = <fs_result>-count + 1.
      ELSE.
        " Add new word to result table
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD split_into_words.
    " Use regular expressions to split the input text into words
    " Regex will split text by any non-alphanumeric characters except apostrophes in contractions
    DATA: lv_pattern TYPE string VALUE `[^\w']+`.

    SPLIT phrase AT lv_pattern INTO TABLE words.
    " Convert all words to lower case to handle case insensitivity
    LOOP AT words INTO lv_word.
      lv_word = to_lower( lv_word ).
      MODIFY TABLE words FROM lv_word TRANSPORTING ALL FIELDS WHERE TABLE_LINE = lv_word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
