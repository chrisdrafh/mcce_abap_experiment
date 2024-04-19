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
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string.
    FIELD-SYMBOLS: <fs_word> TYPE string.

    " Normalize phrase by converting to lower case
    phrase = to_lower( phrase ).

    " Use regular expression to split the phrase into words
    " Words are separated by whitespace or punctuation, but preserve contractions
    SPLIT phrase AT '[^a-z0-9'']+?' INTO TABLE lt_words.

    " Declare a temporary work table to collect counts
    DATA(lt_word_count) = VALUE return_table( ).

    " Loop over the list of words and count each occurrence
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      ASSIGN lv_word TO <fs_word>.
      " Remove any enclosing single quotes that might have been captured
      REPLACE ALL OCCURRENCES OF ''' in <fs_word> WITH ''.

      " Check and increase count for each word
      READ TABLE lt_word_count ASSIGNING FIELD-SYMBOL(<fs_entry>)
        WITH KEY word = <fs_word>.
      IF sy-subrc = 0.
        ADD 1 TO <fs_entry>-count.
      ELSE.
        " Add new word to the count table
        APPEND VALUE #( word = <fs_word> count = 1 ) TO lt_word_count.
      ENDIF.
    ENDLOOP.

    result = lt_word_count.
  ENDMETHOD.
ENDCLASS.
