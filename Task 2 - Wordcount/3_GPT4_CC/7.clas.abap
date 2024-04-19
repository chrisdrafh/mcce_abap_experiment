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
    METHODS split_into_words
      IMPORTING
        !phrase TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lt_result TYPE return_table,
          lv_word TYPE string.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_result ASSIGNING FIELD-SYMBOL(<ls_result>) WITH KEY word = lv_word.
      IF sy-subrc = 0.
        <ls_result>-count = <ls_result>-count + 1.
      ELSE.
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD split_into_words.
    DATA: lt_chars TYPE TABLE OF string,
          lv_char TYPE string.

    SPLIT phrase AT `[^a-zA-Z0-9']` INTO TABLE lt_chars.

    LOOP AT lt_chars INTO lv_char WHERE lv_char IS NOT INITIAL.
      TRANSLATE lv_char TO LOWER CASE.
      APPEND lv_char TO words.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
