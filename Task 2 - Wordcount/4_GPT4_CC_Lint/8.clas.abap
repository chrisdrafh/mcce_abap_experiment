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

    " Normalize the input string: convert to lower case
    phrase = to_lower( phrase ).

    " Replace all punctuation that should separate words (but not apostrophes) with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[\.,\?!:;\"()-]+' IN phrase WITH ' '.
    REPLACE ALL OCCURRENCES OF REGEX '[\s]+' IN phrase WITH space INTO phrase.

    " Split the normalized phrase into a table of words
    SPLIT phrase AT space INTO TABLE lt_words.

    " Initialize result table
    CLEAR result.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE result ASSIGNING <fs_word> WITH KEY word = lv_word.
      IF sy-subrc = 0.
        <fs_word>-count = <fs_word>-count + 1.
      ELSE.
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
