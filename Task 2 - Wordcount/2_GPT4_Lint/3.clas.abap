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
        !phrase       TYPE string
      RETURNING
        VALUE(words)  TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          ls_count TYPE return_structure.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO DATA(lv_word).
      TRY.
          READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            " Word exists, increment count
            MODIFY result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 )
              TRANSPORTING count WHERE word = lv_word.
          ELSE.
            " New word, add to result
            ls_count-word = lv_word.
            ls_count-count = 1.
            APPEND ls_count TO result.
          ENDIF.
      CATCH cx_sy_itab_duplicate_key INTO DATA(lx_duplicate).
          " Handle potential duplicate key exception if occurred
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_into_words.
    " Split input string by punctuation and whitespace excluding apostrophes
    SPLIT phrase AT '[\s,.!?:;"()\-]+' INTO TABLE words.
    " Normalize to lowercase for case insensitivity
    LOOP AT words INTO DATA(lv_word_ref) REFERENCE.
      lv_word_ref->* = to_lower( lv_word_ref->* ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
