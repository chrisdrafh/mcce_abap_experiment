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
    METHODS split_into_words
      IMPORTING
        !input       TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table,
          ls_result LIKE LINE OF lt_result.

    " Split phrase into words based on the defined rules
    lt_words = split_into_words( phrase ).

    " Loop through each word and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE lt_result.
      ELSE.
        READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
        ls_result-count += 1.
        MODIFY TABLE lt_result FROM ls_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD split_into_words.
    " Splits the input string into words, considering punctuation and whitespace
    DATA: lv_phrase TYPE string.
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']+' IN phrase WITH ' ' INTO lv_phrase.
    SPLIT lv_phrase AT ' ' INTO TABLE words.
  ENDMETHOD.

ENDCLASS.
