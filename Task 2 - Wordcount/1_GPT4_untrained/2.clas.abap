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
    METHODS normalize_text
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(normalized_text) TYPE string.

ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string.

    " Normalize and split the input text
    DATA(lv_normalized_text) = normalize_text( phrase ).

    SPLIT lv_normalized_text AT ` ` INTO TABLE lt_words.

    " Use a temporary internal table to store word counts
    TYPES: BEGIN OF ty_word_count,
             word TYPE string,
             count TYPE i,
           END OF ty_word_count.
    DATA: lt_word_count TYPE SORTED TABLE OF ty_word_count
                             WITH UNIQUE KEY word.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE lt_word_count WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY TABLE lt_word_count FROM VALUE #( word = lv_word count = lt_word_count[ word = lv_word ]-count + 1 ).
      ELSE.
        INSERT VALUE #( word = lv_word count = 1 ) INTO TABLE lt_word_count.
      ENDIF.
    ENDLOOP.

    " Map the sorted table to the expected return type
    LOOP AT lt_word_count INTO DATA(ls_word_count).
      APPEND VALUE return_structure( word = ls_word_count-word count = ls_word_count-count ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_text.
    " Convert to lowercase and replace all non-' characters with space
    normalized_text = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9\']' IN normalized_text WITH ` `.
  ENDMETHOD.

ENDCLASS.
