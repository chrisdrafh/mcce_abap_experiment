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
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anag) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    CLEAR result.
    LOOP AT candidates INTO DATA(candidate).
      IF candidate <> input AND is_anagram( input, candidate ).
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA(lv_word1_sorted TYPE string).
    DATA(lv_word2_sorted TYPE string).

    lv_word1_sorted = to_upper( word1 ).
    lv_word2_sorted = to_upper( word2 ).

    SORT lv_word1_sorted.
    SORT lv_word2_sorted.

    is_anag = xsdbool( lv_word1_sorted = lv_word2_sorted ).
  ENDMETHOD.
ENDCLASS.
