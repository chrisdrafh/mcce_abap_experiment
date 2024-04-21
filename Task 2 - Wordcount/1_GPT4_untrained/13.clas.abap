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
        text TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lv_text TYPE string,
          lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_return TYPE return_structure,
          lt_result TYPE return_table.

    lv_text = normalize_text( phrase ).

    " Split the normalized text into words
    SPLIT lv_text AT '[^a-zA-Z0-9'']+' INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      lv_word = to_lower( lv_word ).
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        ls_return-word = lv_word.
        ls_return-count = 1.
        INSERT ls_return INTO TABLE lt_result.
      ELSE.
        MODIFY TABLE lt_result FROM VALUE #( word = lv_word count = ls_return-count + 1 ) FOR lv_word = lv_word.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_text.
    " Replace all punctuations (except apostrophes) with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[[:punct:]&&[^'']]' IN text WITH ' '.
    " Normalize the remaining text
    REPLACE ALL OCCURRENCES OF REGEX '\s+' IN text WITH ' '.
    result = text.
  ENDMETHOD.

ENDCLASS.
