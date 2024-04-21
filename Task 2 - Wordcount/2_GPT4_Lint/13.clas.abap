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
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Convert to lower case for case insensitivity
    phrase = to_lower( phrase ).

    " Use regex to find words, split on punctuation except apostrophes in contractions
    SPLIT phrase AT REGEX '[^a-zA-Z0-9\']+' INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
      IF sy-subrc = 0.
        ls_result-count += 1.
      ELSE.
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO lt_result.
      ENDIF.
      MODIFY TABLE lt_result FROM ls_result.
    ENDLOOP.

    " Return the result table
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
