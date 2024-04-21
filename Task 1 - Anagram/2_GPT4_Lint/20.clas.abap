CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: normalize_and_sort
      IMPORTING
        iv_word       TYPE string
      RETURNING
        VALUE(rv_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lt_result           TYPE string_table.

    lv_normalized_input = normalize_and_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_and_sort( lv_candidate ) = lv_normalized_input AND lv_candidate <> input.
        APPEND lv_candidate TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_and_sort.
    " Normalize by converting to lower case and sorting the characters
    DATA: lt_chars TYPE TABLE OF char,
          lv_char  TYPE char.

    TRANSLATE iv_word TO LOWER CASE.
    DO strlen( iv_word ) TIMES.
      lv_char = iv_word+sy-index(1).
      APPEND lv_char TO lt_chars.
    ENDDO.
    SORT lt_chars.
    CLEAR rv_word.
    LOOP AT lt_chars INTO lv_char.
      CONCATENATE rv_word lv_char INTO rv_word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
