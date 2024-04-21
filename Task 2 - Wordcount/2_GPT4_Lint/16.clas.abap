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
    METHODS split_phrase_into_words
      IMPORTING
        phrase TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lt_result TYPE return_table.

    lt_words = split_phrase_into_words( phrase ).

    LOOP AT lt_words INTO DATA(lv_word).
      lv_word = to_lower( lv_word ). " Convert word to lower case for case insensitivity
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word exists, increase count
        MODIFY lt_result WHERE word = lv_word ASSIGNING FIELD-SYMBOL(<fs_word>).
        <fs_word>-count = <fs_word>-count + 1.
      ELSE.
        " New word, add to table
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD split_phrase_into_words.
    " This method splits the phrase into words considering contractions and removes punctuation
    SPLIT phrase AT '\n' INTO TABLE words.
    DATA(lv_temp) = VALUE string_table( FOR <word> IN words ( 
                         FOR <line> IN GROUPS OF cl_abap_regex=>create_posix( '[^a-zA-Z0-9'']+' )->split( <word> ) 
                         WHERE line IS NOT INITIAL ( line ) ) ).

    CLEAR words. " Clear initial split table
    words = lv_temp.
  ENDMETHOD.

ENDCLASS.
