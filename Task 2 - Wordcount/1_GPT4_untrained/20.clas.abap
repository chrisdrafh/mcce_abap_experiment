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
      return_table TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS split_string
      IMPORTING
        value TYPE string
      RETURNING
        VALUE(result) TYPE string_table.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE string_table,
          lv_word TYPE string.
    FIELD-SYMBOLS: <fs_word> TYPE return_structure.

    lt_words = split_string( phrase ).

    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      READ TABLE result ASSIGNING <fs_word> WITH KEY word = lv_word.
      IF sy-subrc = 0.
        ADD 1 TO <fs_word>-count.
      ELSE.
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_string.
    DATA: lt_chars TYPE string_table.
    lt_chars = value `[^0-9a-zA-Z']+` INTO COMPONENTS OF value.
    RETURN VALUE #( FOR <fs_char> IN lt_chars WHERE <fs_char> IS NOT INITIAL ( <fs_char> ) ).
  ENDMETHOD.

ENDCLASS.
