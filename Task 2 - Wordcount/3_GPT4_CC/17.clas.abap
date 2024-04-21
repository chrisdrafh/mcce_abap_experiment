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
      return_table TYPE HASHED TABLE OF return_structure WITH UNIQUE KEY word.
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
          lv_word TYPE string.

    " Replace punctuation with spaces, but preserve apostrophes in contractions
    REPLACE ALL OCCURRENCES OF REGEX '[\.,:;!?"]' IN phrase WITH ' '.
    SPLIT phrase AT space INTO TABLE lt_words.

    " Prepare result table with hashed key for quick access
    result = VALUE #( ).

    LOOP AT lt_words INTO lv_word.
      " Normalize the word to lowercase to ensure case insensitivity
      lv_word = to_lower( lv_word ).

      " Ignore empty tokens which can occur from consecutive delimiters
      IF lv_word IS NOT INITIAL.
        " Increment count of each word
        READ TABLE result ASSIGNING FIELD-SYMBOL(<fs_result>) WITH KEY word = lv_word.
        IF sy-subrc = 0.
          ADD 1 TO <fs_result>-count.
        ELSE.
          result = VALUE #( BASE result ( word = lv_word count = 1 ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
