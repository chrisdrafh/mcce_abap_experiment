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
          lv_word TYPE string.

    " Split phrase into words by whitespace and punctuation (except apostrophes)
    " Regex to match words including contractions and numbers
    SPLIT phrase AT '[^a-zA-Z0-9\']+'
        INTO TABLE lt_words.

    " Using a local internal table to count occurrences
    DATA: lt_counts TYPE return_table.

    " Loop over the list of words
    LOOP AT lt_words INTO lv_word.
      " Convert word to lower case
      lv_word = to_lower( lv_word ).

      " Skip empty strings resulting from consecutive delimiters
      IF lv_word IS NOT INITIAL.
        " Check if word is already in the table
        READ TABLE lt_counts ASSIGNING FIELD-SYMBOL(<ls_count>) WITH KEY word = lv_word.
        IF sy-subrc = 0.
          " If found, increment count
          <ls_count>-count = <ls_count>-count + 1.
        ELSE.
          " If not found, add new entry
          APPEND VALUE #( word = lv_word count = 1 ) TO lt_counts.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Return the result
    result = lt_counts.
  ENDMETHOD.

ENDCLASS.
