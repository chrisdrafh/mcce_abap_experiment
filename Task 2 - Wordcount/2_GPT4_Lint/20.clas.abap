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
        VALUE(result) TYPE return_table .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          ls_result LIKE LINE OF result.

    " Use Regular Expression to split the phrase into words
    " Handle ASCII punctuation as delimiters except for apostrophes in contractions
    SPLIT phrase AT '[^a-zA-Z0-9'']+' INTO TABLE lt_words.

    " Convert words to lowercase to handle case insensitivity
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).

      READ TABLE result INTO ls_result WITH KEY word = lv_word.
      IF sy-subrc = 0.
        " Word exists, increment count
        ls_result-count = ls_result-count + 1.
        MODIFY result FROM ls_result.
      ELSE.
        " New word, add to result
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
