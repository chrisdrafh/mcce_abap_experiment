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
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result,
          lv_phrase TYPE string.

    " Normalize input phrase
    lv_phrase = to_lower( phrase ).

    " Use Regular Expressions to split input into words
    " Matches words which may include internal apostrophes (contractions)
    FIND ALL OCCURRENCES OF REGEX '[\w]+' IN lv_phrase MATCHING ALL COMPONENTS INTO lt_words.

    " Count each word occurrence
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Increment count if the word exists
        READ TABLE lt_result INTO ls_result WITH KEY word = lv_word.
        ls_result-count = ls_result-count + 1.
        MODIFY lt_result FROM ls_result TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " Add new word with count 1 if not found
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.
ENDCLASS.
