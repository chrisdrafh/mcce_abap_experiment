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
      return_table TYPE STANDARD TABLE OF return_structure WITH KEY word.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lv_text TYPE string,
          lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result TYPE return_structure,
          lt_result TYPE HASHED TABLE OF return_structure WITH UNIQUE KEY word.

    " Normalize the phrase to lowercase to ensure case insensitivity
    lv_text = to_lower( phrase ).

    " Replace non-word characters (punctuation) with space, preserve contractions (apostrophes in words)
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']+' IN lv_text WITH ' '.

    " Split the normalized text into words
    SPLIT lv_text AT ' ' INTO TABLE lt_words.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word INTO ls_result.
        IF sy-subrc = 0.
          ls_result-count += 1.
        ELSE.
          ls_result-word = lv_word.
          ls_result-count = 1.
        ENDIF.
        INSERT ls_result INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    " Move the hashed table results into the standard table for return
    LOOP AT lt_result INTO ls_result.
      APPEND ls_result TO result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
