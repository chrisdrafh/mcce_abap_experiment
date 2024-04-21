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
    DATA: lv_phrase TYPE string.
    DATA: lt_words TYPE STANDARD TABLE OF string.
    DATA: lt_result TYPE return_table.
    DATA: lv_word TYPE string.
    DATA: lv_word_clean TYPE string.
    DATA: lv_char TYPE c LENGTH 1.
    DATA: ls_result TYPE return_structure.

    " Convert to lower case for case insensitivity
    lv_phrase = to_lower( phrase ).

    " Process the phrase character by character to handle punctuations
    DO strlen( lv_phrase ) TIMES.
      lv_char = lv_phrase+sy-index(1).
      CASE lv_char.
        WHEN `a` TO `z` OR `0` TO `9` OR `'`. " Include alphabets, numbers, and apostrophe
          CONCATENATE lv_word lv_char INTO lv_word.
        WHEN OTHERS.
          IF lv_word IS NOT INITIAL.
            " Remove trailing apostrophes from words
            lv_word_clean = lv_word.
            IF lv_word_clean CP '*'''. " Check if it ends with an apostrophe
              lv_word_clean = lv_word( strlen( lv_word ) - 1 ).
            ENDIF.
            " Collect word
            COLLECT VALUE #( word = lv_word_clean count = 1 ) INTO lt_result.
            CLEAR lv_word.
          ENDIF.
      ENDCASE.
    ENDDO.

    " Check for last word if no trailing punctuation
    IF lv_word IS NOT INITIAL.
      " Remove trailing apostrophes
      lv_word_clean = lv_word.
      IF lv_word_clean CP '*'''. " Check if it ends with an apostrophe
        lv_word_clean = lv_word( strlen( lv_word ) - 1 ).
      ENDIF.
      COLLECT VALUE #( word = lv_word_clean count = 1 ) INTO lt_result.
    ENDIF.

    " Aggregate counts
    LOOP AT lt_result INTO ls_result GROUP BY ls_result-word
        ASCENDING ASSIGNING FIELD-SYMBOL(<fs_result>).
      result = VALUE #( BASE result ( word = <fs_result>-word count = GROUP SIZE ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
