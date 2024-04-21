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
    METHODS to_lower_case
      IMPORTING
        !iv_text TYPE string
      RETURNING
        VALUE(rv_text) TYPE string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_word TYPE string,
          lt_result TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY,
          ls_result LIKE LINE OF lt_result.

    " Convert input to lower case for case insensitivity
    phrase = to_lower_case( phrase ).

    " Replace all non-word and non-apostrophe characters with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN phrase WITH ' '.

    " Split the modified phrase into words
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Count each word occurrence
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
      IF sy-subrc = 0.
        ls_result-count = ls_result-count + 1.
        MODIFY lt_result FROM ls_result.
      ELSE.
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    " Prepare result
    result = lt_result.

  ENDMETHOD.

  METHOD to_lower_case.
    rv_text = to_lower( iv_text ).
  ENDMETHOD.

ENDCLASS.
