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
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string.
    FIELD-SYMBOLS: <lv_word> TYPE string.
    DATA(lv_regex) TYPE string VALUE `[\w']{1,}`. " Regex pattern to match words including contractions

    " Normalize case
    phrase = to_lower( phrase ).

    " Replace all non-word characters except apostrophe with space to simplify split
    REPLACE ALL OCCURRENCES OF REGEX '[^\w']+' IN phrase WITH ' '.

    " Split phrase into words
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Initialize result table
    DATA(lt_result) TYPE return_table.

    " Count occurrences
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        " Check for single apostrophes not as part of contractions and skip
        IF lv_word CP '*''*'.
          CONTINUE.
        ENDIF.

        " Increment count for the word
        READ TABLE lt_result ASSIGNING <lv_word> WITH KEY word = lv_word.
        IF sy-subrc = 0.
          <lv_word>-count += 1.
        ELSE.
          APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

ENDCLASS.
