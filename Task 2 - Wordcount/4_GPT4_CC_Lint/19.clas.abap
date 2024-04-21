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
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lv_phrase TYPE string.
    DATA: lt_words TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA: lv_word TYPE string.
    DATA: ls_result LIKE LINE OF result.

    " Normalize the input to handle case insensitivity
    lv_phrase = to_lower( phrase ).

    " Split the phrase into words based on punctuation and whitespace, excluding apostrophes
    SPLIT lv_phrase AT '[^a-z0-9'']+' INTO TABLE lt_words.

    " Count each unique word
    LOOP AT lt_words INTO lv_word.
      " Check for empty or meaningless strings resulted from split
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ls_result-word = lv_word.
          ls_result-count = 1.
          APPEND ls_result TO result.
        ELSE.
          READ TABLE result WITH KEY word = lv_word INTO ls_result.
          ADD 1 TO ls_result-count.
          MODIFY result FROM ls_result.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
