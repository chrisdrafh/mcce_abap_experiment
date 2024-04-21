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
        VALUE(rv_normalized) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string.
    lv_normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_normalized_input = normalize_word( lv_candidate )
          AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    rv_normalized = to_upper( iv_word ). " Normalize by converting to upper case
    SORT rv_normalized BY ( val = rv_normalized INTO lv_char ) ASCENDING AS TEXT.
  ENDMETHOD.
ENDCLASS.
