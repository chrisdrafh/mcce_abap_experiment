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
        !text TYPE string
      RETURNING
        VALUE(normalized_text) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lv_normalized_text TYPE string.

    lv_normalized_text = normalize_text( phrase ).

    " Split normalized text into words
    SPLIT lv_normalized_text AT ' ' INTO TABLE lt_words.

    " Loop through words and count occurrences
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word found, increase count
        MODIFY result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 ) TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " New word, add to table
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_text.
    " Replace punctuation with space, except apostrophes and normalize case
    normalized_text = to_lower( text ).
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN normalized_text WITH ' '.
  ENDMETHOD.

ENDCLASS.
