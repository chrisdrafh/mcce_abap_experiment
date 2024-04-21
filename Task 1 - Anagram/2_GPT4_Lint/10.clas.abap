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
    METHODS normalize_word
      IMPORTING
        iv_word       TYPE string
      RETURNING
        VALUE(rv_normalized_word) TYPE string.

    METHODS is_anagram
      IMPORTING
        iv_word1      TYPE string
        iv_word2      TYPE string
      RETURNING
        VALUE(rv_is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string.
    lv_normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF is_anagram( lv_normalized_input, lv_candidate ) = abap_true.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    rv_normalized_word = to_lower( iv_word ).
    SORT rv_normalized_word BY ( byte ).
  ENDMETHOD.

  METHOD is_anagram.
    DATA(lv_normalized_word1) = normalize_word( iv_word1 ).
    DATA(lv_normalized_word2) = normalize_word( iv_word2 ).

    rv_is_anagram = xsdbool( lv_normalized_word1 = lv_normalized_word2 AND iv_word1 <> iv_word2 ).
  ENDMETHOD.
ENDCLASS.
