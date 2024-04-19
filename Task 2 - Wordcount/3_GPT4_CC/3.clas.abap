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
          lt_result TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    " Normalize input to lower case
    phrase = to_lower( phrase ).

    " Use regular expressions to split the input into words, handling contractions correctly
    FIND ALL OCCURRENCES OF REGEX '[\w'']+' IN phrase MATCHING SUBSTRINGS INTO lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      IF lv_word CS ''''.
        " Handle contractions as single words
        IF NOT line_exists( lt_result[ lv_word ] ).
          INSERT VALUE #( table_line = lv_word, 1 ) INTO TABLE lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).
        ELSE.
          ADD 1 TO <fs_result>.
        ENDIF.
      ELSE.
        " Regular word processing
        TRANSLATE lv_word USING ''' '.
        SPLIT lv_word AT space INTO TABLE lt_words.
        LOOP AT lt_words INTO lv_word.
          IF NOT line_exists( lt_result[ lv_word ] ).
            INSERT VALUE #( table_line = lv_word, 1 ) INTO TABLE lt_result ASSIGNING <fs_result>.
          ELSE.
            ADD 1 TO <fs_result>.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    " Convert result to return structure
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_count>).
      APPEND VALUE #( word = <fs_count>-table_line, count = <fs_count> ) TO result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
