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
          lt_result TYPE return_table,
          ls_result_line LIKE LINE OF lt_result.

    " Normalize the input phrase: convert to lower case and replace non-word characters
    phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9\']' IN phrase WITH ' '.

    " Split phrase into words
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      " Remove any leading or trailing spaces
      CONDENSE lv_word NO-GAPS.
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          ls_result_line-word = lv_word.
          ls_result_line-count = 1.
          INSERT ls_result_line INTO TABLE lt_result.
        ELSE.
          READ TABLE lt_result INTO ls_result_line WITH KEY word = lv_word.
          ls_result_line-count = ls_result_line-count + 1.
          MODIFY lt_result FROM ls_result_line TRANSPORTING count WHERE word = lv_word.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.

  ENDMETHOD.
ENDCLASS.
