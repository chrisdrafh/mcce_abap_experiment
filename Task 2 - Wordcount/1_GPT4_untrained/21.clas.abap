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
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Use regular expression to split the input phrase into words
    " We use \W+ to split by non-word characters except for apostrophes in contractions
    SPLIT phrase AT '\W+?(?<!\S'\w)' INTO TABLE lt_words.

    " Convert words to lower case and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ). " Convert to lowercase for case insensitivity

      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word exists, increment count
        MODIFY lt_result FROM VALUE #( word = lv_word count = ls_result.count + 1 ) WHERE word = lv_word.
      ELSE.
        " If new word, add to result table
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    " Assign result to output
    result = lt_result.

  ENDMETHOD.

ENDCLASS.
