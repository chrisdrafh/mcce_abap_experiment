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
    METHODS normalize_string
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(rv_normalized) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_normalized TYPE string.
    lv_input_normalized = normalize_string( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      DATA(lv_candidate_normalized) = normalize_string( lv_candidate ).

      IF lv_input_normalized = lv_candidate_normalized AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    rv_normalized = to_lower( iv_string ). " Convert to lowercase
    SORT rv_normalized BY (val = lv_char) ASCENDING AS TEXT. " Sort characters
  ENDMETHOD.

ENDCLASS.
