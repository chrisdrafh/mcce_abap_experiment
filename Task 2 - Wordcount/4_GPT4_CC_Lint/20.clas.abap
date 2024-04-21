CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result LIKE LINE OF result.

    " Normalize the input phrase to handle case insensitivity
    phrase = to_lower( phrase ).

    " Replace all punctuations that are not apostrophes or alphanumeric with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN phrase WITH ' '.

    " Split the phrase into words based on spaces
    SPLIT phrase AT space INTO TABLE lt_words.

    " Initialize the result table
    CLEAR: result.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      " Skip empty words which might be created by multiple spaces
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          " Add new word with count 1 if not found
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE result.
        ELSE.
          " Increment count if already present
          MODIFY TABLE result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 ) WHERE word = lv_word.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
