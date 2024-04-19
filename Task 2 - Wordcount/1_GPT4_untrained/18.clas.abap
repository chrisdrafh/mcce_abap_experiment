CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
    METHODS split_into_words
      IMPORTING
        !input_phrase TYPE string
      RETURNING
        VALUE(words) TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_word  TYPE string.

    lt_words = split_into_words( phrase ).

    CLEAR result.
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word is found, increase count
        MODIFY result ASSIGNING FIELD-SYMBOL(<fs_result>) WHERE word = lv_word.
        IF sy-subrc = 0.
          <fs_result>-count = <fs_result>-count + 1.
        ENDIF.
      ELSE.
        " If word is not found, add it with count 1
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_into_words.
    DATA: lt_punctuation TYPE TABLE OF string WITH DEFAULT KEY,
          lv_regex      TYPE string.

    lt_punctuation = VALUE #(
      ( `.` ) ( `,` ) ( `;` ) ( `:` ) ( `!` ) ( `?` ) ( `"`) ( `(` ) ( `)` )
      ( `[` ) ( `]` ) ( `{` ) ( `}` ) ( `-` ) ( `\n` ) ( `\t` ) ( `' '` )
    ).

    " Create regex pattern, do not split on apostrophe
    lv_regex = `[^\w']+`.

    " Replace punctuations with space (except apostrophe)
    REPLACE ALL OCCURRENCES OF REGEX lv_regex IN input_phrase WITH SPACE.
    " Convert to lowercase for case insensitivity
    TRANSLATE input_phrase TO LOWER CASE.

    SPLIT input_phrase AT SPACE INTO TABLE words.

  ENDMETHOD.

ENDCLASS.
