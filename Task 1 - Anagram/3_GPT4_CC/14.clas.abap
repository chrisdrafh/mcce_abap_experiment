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
    METHODS prepare_word
      IMPORTING
        iv_word    TYPE string
      RETURNING
        VALUE(rv_word) TYPE string.

    METHODS are_anagrams
      IMPORTING
        iv_word1 TYPE string
        iv_word2 TYPE string
      RETURNING
        VALUE(rv_match) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_prepared_input TYPE string.
    lv_prepared_input = prepare_word( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF are_anagrams( lv_prepared_input, prepare_word( lv_candidate ) ) AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_word.
    rv_word = iv_word.
    TRANSLATE rv_word TO UPPER CASE.
    SORT rv_word AS TEXT INTO rv_word.
  ENDMETHOD.

  METHOD are_anagrams.
    rv_match = abap_true.

    IF iv_word1 <> iv_word2.
      rv_match = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
