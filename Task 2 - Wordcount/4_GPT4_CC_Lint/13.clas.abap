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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS to_lower_case
      IMPORTING
        !value TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word  TYPE string.
    FIELD-SYMBOLS: <lv_word> TYPE string.

    " Split the phrase into words
    SPLIT phrase AT `[^\w']+` INTO TABLE lt_words.

    " Prepare a local structure to count occurrences
    DATA: lt_word_count TYPE return_table.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower_case( lv_word ).

      READ TABLE lt_word_count ASSIGNING <lv_word> WITH KEY word = lv_word.
      IF sy-subrc = 0.
        <lv_word>-count = <lv_word>-count + 1.
      ELSE.
        APPEND VALUE #( word = lv_word count = 1 ) TO lt_word_count.
      ENDIF.
    ENDLOOP.

    result = lt_word_count.
  ENDMETHOD.

  METHOD to_lower_case.
    result = to_lower( value ).
  ENDMETHOD.

ENDCLASS.
