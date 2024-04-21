CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
    DATA: lv_phrase TYPE string,
          lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result LIKE LINE OF result,
          lt_result TYPE return_table.

    lv_phrase = to_lower( phrase ).

    " Use regex to split the input text into words, considering contractions
    FIND ALL OCCURRENCES OF REGEX '[[:alpha:]]+\'?[[:alpha:]]*|[[:digit:]]+' IN lv_phrase MATCHING SUBSTRINGS INTO lt_words.

    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result ASSIGNING FIELD-SYMBOL(<fs_result>) WITH KEY word = lv_word.
      IF sy-subrc = 0.
        " If word exists, increment count
        ADD 1 TO <fs_result>-count.
      ELSE.
        " Else, initialize and insert new word with count 1
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
