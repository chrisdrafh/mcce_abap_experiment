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
    DATA: lt_words TYPE TABLE OF string,
          ls_count TYPE return_structure.
    FIELD-SYMBOLS: <fs_word> TYPE string.

    " Regular expression to split input into words
    SPLIT phrase AT '[^a-zA-Z0-9\']+|\s+' INTO TABLE lt_words.

    " Initialize local hash table to count words
    DATA(lt_word_count) = VALUE return_table( ).

    " Process each word in the list
    LOOP AT lt_words INTO DATA(lv_word).
      lv_word = to_lower( lv_word ). " Convert word to lower case to ensure case-insensitivity

      " Increment the count for this word
      READ TABLE lt_word_count ASSIGNING <fs_word> WITH KEY word = lv_word.
      IF sy-subrc = 0.
        <fs_word>-count = <fs_word>-count + 1.
      ELSE.
        ls_count-word = lv_word.
        ls_count-count = 1.
        INSERT ls_count INTO TABLE lt_word_count.
      ENDIF.
    ENDLOOP.

    " Return the result
    result = lt_word_count.
  ENDMETHOD.

ENDCLASS.
