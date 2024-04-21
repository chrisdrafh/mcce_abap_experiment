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
    METHODS sort_and_lowercase
      IMPORTING
        VALUE(iv_word) TYPE string
      RETURNING
        VALUE(rv_normalized_word) TYPE string.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_normalized TYPE string.

    lv_input_normalized = sort_and_lowercase( iv_word = input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND sort_and_lowercase( lv_candidate ) = lv_input_normalized.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_and_lowercase.
    DATA: lv_word TYPE string.
    lv_word = to_lower( iv_word ). " Convert to lowercase
    SORT lv_word BY ( val = %_CAST( string, substring( lv_word, sy-index, 1 ) ) ).
    rv_normalized_word = lv_word.
  ENDMETHOD.

ENDCLASS.
