CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word  TYPE string,
          ls_result TYPE return_structure.

    FIELD-SYMBOLS: <lv_word> LIKE LINE OF lt_words.

    " Normalize input: lower case and replace punctuation
    phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^\w''']+'
              IN phrase WITH ' '
              IGNORING CASE.

    " Split the phrase into words
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words ASSIGNING <lv_word>.
      IF <lv_word> IS NOT INITIAL.
        READ TABLE result WITH KEY word = <lv_word> INTO ls_result.
        IF sy-subrc = 0.
          ls_result-count = ls_result-count + 1.
          MODIFY result FROM ls_result TRANSPORTING count WHERE word = <lv_word>.
        ELSE.
          ls_result-word = <lv_word>.
          ls_result-count = 1.
          APPEND ls_result TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
