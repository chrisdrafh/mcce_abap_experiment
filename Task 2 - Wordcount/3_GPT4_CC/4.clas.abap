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
    DATA: lv_phrase_lower TYPE string.
    DATA: lt_words TYPE TABLE OF string.
    DATA: lv_word TYPE string.
    DATA: lt_word_count TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    DATA: ls_return TYPE return_structure.

    " Normalize case
    lv_phrase_lower = to_lower( phrase ).

    " Split words considering contractions and punctuation
    SPLIT lv_phrase_lower AT '\([^a-zA-Z0-9''\-]\)+' INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL AND lv_word CO_MATCHES '^[a-zA-Z0-9''\-]+$'.
        ADD 1 TO lt_word_count[ lv_word ].
      ENDIF.
    ENDLOOP.

    " Prepare return table
    LOOP AT lt_word_count ASSIGNING FIELD-SYMBOL(<fs_word_count>).
      ls_return-word = <fs_word_count>-table_line.
      ls_return-count = <fs_word_count>.
      APPEND ls_return TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
