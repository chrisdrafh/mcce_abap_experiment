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
    DATA: lv_normalized_input TYPE string.
    
    lv_normalized_input = normalize_string( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_string( lv_candidate ) = lv_normalized_input AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    rv_normalized = to_lower( iv_string ).
    SORT rv_normalized BY COMPONENTS AS TEXT.
  ENDMETHOD.
ENDCLASS.
