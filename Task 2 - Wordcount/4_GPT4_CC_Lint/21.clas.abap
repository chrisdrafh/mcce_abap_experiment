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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS split_into_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(words)  TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          ls_result LIKE LINE OF result.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO DATA(lv_word).
      READ TABLE result INTO ls_result WITH KEY word = lv_word.
      IF sy-subrc = 0.
        ls_result-count += 1.
      ELSE.
        ls_result-word = lv_word.
        ls_result-count = 1.
      ENDIF.
      INSERT ls_result INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_into_words.
    DATA: lv_phrase TYPE string.
    lv_phrase = to_lower( phrase ). " Convert to lowercase for case insensitivity
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9\']' IN lv_phrase WITH SPACE. " Replace non-word and non-apostrophe chars with space
    SPLIT lv_phrase AT ' ' INTO TABLE words. " Split at spaces to get words
  ENDMETHOD.

ENDCLASS.
