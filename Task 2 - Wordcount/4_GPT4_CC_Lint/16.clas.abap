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
    METHODS normalize_text
      IMPORTING
        !text       TYPE string
      RETURNING
        VALUE(clean_text) TYPE string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lv_phrase TYPE string,
          lt_words TYPE STANDARD TABLE OF string,
          ls_word TYPE string,
          lv_clean_text TYPE string.

    FIELD-SYMBOLS: <lv_word> TYPE string.

    lv_phrase = phrase.
    lv_clean_text = normalize_text( lv_phrase ).

    " Split the cleaned text into words based on whitespace
    SPLIT lv_clean_text AT '\s+' INTO TABLE lt_words.

    " Initialize the result table
    CLEAR: result.

    " Count occurrences of each word
    LOOP AT lt_words ASSIGNING <lv_word>.
      READ TABLE result ASSIGNING FIELD-SYMBOL(<ls_result>)
        WITH KEY word = <lv_word>.
      IF sy-subrc = 0.
        ADD 1 TO <ls_result>-count.
      ELSE.
        ls_word-word = <lv_word>.
        ls_word-count = 1.
        INSERT ls_word INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_text.
    " Normalize text by converting to lowercase and replacing punctuation
    clean_text = to_lower( text ).
    REPLACE ALL OCCURRENCES OF REGEX '[^\w\']' IN clean_text WITH ' '.
    " Keep contractions by not splitting on apostrophes
    " Transform multiple spaces to single space for uniform splitting later
    REPLACE ALL OCCURRENCES OF REGEX '\s+' IN clean_text WITH ' '.
  ENDMETHOD.

ENDCLASS.
