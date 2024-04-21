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
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Normalize the phrase to lowercase
    phrase = to_lower( phrase ).

    " Split words using regex that considers punctuation as delimiters except for apostrophes
    SPLIT phrase AT '[^a-z0-9\']+' INTO TABLE lt_words.

    " Count each word occurrence
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Increment count if word exists
          lt_result[ sy-tabix ]-count = lt_result[ sy-tabix ]-count + 1.
        ELSE.
          " Add new word with count 1
          ls_result-word  = lv_word.
          ls_result-count = 1.
          APPEND ls_result TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Prepare result table
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
