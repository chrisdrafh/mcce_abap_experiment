CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS normalize_word
      IMPORTING
        iv_word       TYPE string
      RETURNING
        VALUE(rv_normalized_word) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    lv_normalized_input = normalize_word( iv_word = input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      lv_normalized_candidate = normalize_word( iv_word = lv_candidate ).
      IF lv_normalized_candidate = lv_normalized_input AND lv_candidate <> input.
        CONTINUE. " Skip identical words
      ENDIF.
      IF lv_normalized_candidate = lv_normalized_input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_word.
    " Convert the input word to lowercase and sort the characters
    rv_normalized_word = lv_candidate.
    TRANSLATE rv_normalized_word TO LOWER CASE.
    SORT rv_normalized_word AS TEXT.
  ENDMETHOD.

ENDCLASS.
