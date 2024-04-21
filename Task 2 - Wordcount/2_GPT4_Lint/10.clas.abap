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
          ls_result LIKE LINE OF lt_result.

    " Normalize the input string to lower case and replace non-word characters except apostrophe with space
    phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9\']' IN phrase WITH space.

    " Split the normalized string into words
    SPLIT phrase AT space INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE lt_result.
        ELSE.
          READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
          ls_result-count = ls_result-count + 1.
          MODIFY lt_result FROM ls_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Sort the result table alphabetically by word
    SORT lt_result BY word.

    " Return the result
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
