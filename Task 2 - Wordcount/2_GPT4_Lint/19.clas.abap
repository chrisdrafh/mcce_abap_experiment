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
      return_table TYPE STANDARD TABLE OF return_structure WITH KEY word INITIAL SIZE 0.
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
          lv_word TYPE string,
          lv_cleaned_phrase TYPE string.
    FIELD-SYMBOLS: <fs_word> TYPE string.

    " Normalize the string: remove non-essential punctuation and make it lowercase
    lv_cleaned_phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9''\s]' IN lv_cleaned_phrase WITH ''.

    " Split the cleaned string into words based on spaces and other whitespace characters
    SPLIT lv_cleaned_phrase AT '\s+' INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE result TO <fs_word>.
      IF sy-subrc <> 0.
        INSERT VALUE #( word = lv_word, count = 1 ) INTO TABLE result.
      ELSE.
        <fs_word>-count = <fs_word>-count + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
