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
          lv_word  TYPE string,
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Use regex to split the input phrase by punctuation and spaces, excluding apostrophes in contractions
    SPLIT phrase AT '[\s\.,;:"?!()<>[\]{}|-]+' INTO TABLE lt_words.

    " Loop through each word to normalize and count
    LOOP AT lt_words INTO lv_word.
      " Convert word to lower case for case-insensitivity
      lv_word = to_lower( lv_word ).

      " Skip empty strings resulting from multiple delimiters
      IF lv_word IS NOT INITIAL.
        " Check if the word already exists in the result table
        READ TABLE lt_result INTO ls_result WITH KEY word = lv_word.
        IF sy-subrc = 0.
          " Word exists, increment count
          ls_result-count = ls_result-count + 1.
          MODIFY lt_result FROM ls_result.
        ELSE.
          " New word, add to table
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
