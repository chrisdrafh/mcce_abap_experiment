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
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Use regular expression to split the input into words
    SPLIT phrase AT '\W+|_' INTO TABLE lt_words.

    " Convert words to lower case and count their occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).

      " Skip empty strings resulting from split
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Increase count if word exists
          ls_result-word = lv_word.
          READ TABLE lt_result INTO ls_result WITH KEY word = lv_word.
          ls_result-count = ls_result-count + 1.
          MODIFY lt_result FROM ls_result.
        ELSE.
          " Add new word with count 1 if it does not exist
          ls_result-word = lv_word.
          ls_result-count = 1.
          APPEND ls_result TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
