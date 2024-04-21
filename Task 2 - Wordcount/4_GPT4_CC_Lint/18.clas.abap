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
    METHODS remove_special_characters
      IMPORTING
        !text       TYPE string
      RETURNING
        VALUE(clean_text) TYPE string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word  TYPE string,
          ls_result_line TYPE return_structure,
          lt_result TYPE return_table.

    " Remove punctuation and split the text into words
    lt_words = SPLIT( remove_special_characters( phrase ) AT space INTO TABLE ).

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      " Convert word to lowercase to handle case insensitivity
      lv_word = to_lower( lv_word ).
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " Word not found in the table, add new entry
        ls_result_line-word = lv_word.
        ls_result_line-count = 1.
        INSERT ls_result_line INTO TABLE lt_result.
      ELSE.
        " Word found, increment count
        READ TABLE lt_result WITH KEY word = lv_word INTO ls_result_line.
        ls_result_line-count += 1.
        MODIFY lt_result FROM ls_result_line.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD remove_special_characters.
    " Replace all non-alphanumeric characters (excluding apostrophes) with spaces
    clean_text = replace_all( val = text regex = `[^a-zA-Z0-9']` with = ` ` ).
  ENDMETHOD.

ENDCLASS.
