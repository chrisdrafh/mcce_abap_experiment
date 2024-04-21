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
    DATA: lt_words TYPE TABLE OF string.
    DATA: lv_word TYPE string.
    DATA: ls_result LIKE LINE OF result.
    DATA: lv_regex TYPE string VALUE '[\s!"#$%&()*+,-./:;<=>?@\[\\\]^_`{|}~\n\r\t]+'.
    FIELD-SYMBOLS: <lv_table_line> LIKE LINE OF result.

    " Split input into words, considering contractions
    SPLIT phrase AT lv_regex INTO TABLE lt_words.

    " Convert to lowercase to handle case insensitivity
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      READ TABLE result ASSIGNING <lv_table_line> WITH KEY word = lv_word.
      IF sy-subrc = 0.
        <lv_table_line>-count = <lv_table_line>-count + 1.
      ELSE.
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
