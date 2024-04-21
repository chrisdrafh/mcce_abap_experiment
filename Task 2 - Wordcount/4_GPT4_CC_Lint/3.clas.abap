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
    DATA: lv_phrase TYPE string.
    DATA: lt_parts TYPE TABLE OF string.
    DATA: ls_result LIKE LINE OF result.
    DATA: lt_words TYPE TABLE OF string.
    FIELD-SYMBOLS: <word> TYPE string.

    " Normalize input to lowercase and replace punctuation with spaces, keeping apostrophes
    lv_phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[\[\]\{\}\(\)\.,;:!?"\n\t-]+' IN lv_phrase WITH ' '.
    SPLIT lv_phrase AT ' ' INTO TABLE lt_parts.

    " Process each word, skipping empty strings
    LOOP AT lt_parts INTO DATA(lv_part).
      IF lv_part IS NOT INITIAL.
        " Check if the word is already in the table
        READ TABLE result ASSIGNING <word> WITH KEY word = lv_part.
        IF sy-subrc = 0.
          " Increase count if word exists
          <word>-count = <word>-count + 1.
        ELSE.
          " Add new word with count 1
          ls_result-word = lv_part.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE result.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
