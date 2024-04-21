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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lt_final TYPE return_table.

    " Replace punctuations that are not apostrophes with space and split the string
    REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9'']' IN phrase WITH space.
    SPLIT phrase AT space INTO TABLE lt_words.

    " Convert to lowercase to handle case insensitivity
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      READ TABLE lt_final ASSIGNING FIELD-SYMBOL(<fs_final>) WITH KEY word = lv_word.
      IF sy-subrc = 0.
        " Increment count if word exists
        <fs_final>-count = <fs_final>-count + 1.
      ELSE.
        " Add new word with count 1
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_final.
      ENDIF.
    ENDLOOP.

    result = lt_final.
  ENDMETHOD.

ENDCLASS.
