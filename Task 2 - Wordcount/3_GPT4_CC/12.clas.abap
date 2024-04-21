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
        VALUE(result) TYPE return_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.
  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string.
    DATA: lv_word TYPE string.
    DATA: ls_result LIKE LINE OF result.

    " Normalize input to handle case insensitivity
    phrase = to_lower( phrase ).

    " Replace all non-word characters except apostrophes (for contractions) with space
    REPLACE ALL OCCURRENCES OF REGEX '[^a-z0-9'']' IN phrase WITH ' '.

    " Split the normalized string into words
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Loop through the table and count occurrences
    LOOP AT lt_words INTO lv_word WHERE lv_word IS NOT INITIAL.
      READ TABLE result INTO ls_result WITH KEY word = lv_word.
      IF sy-subrc = 0.
        ls_result-count += 1.
        MODIFY result FROM ls_result.
      ELSE.
        ls_result-word = lv_word.
        ls_result-count = 1.
        APPEND ls_result TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
