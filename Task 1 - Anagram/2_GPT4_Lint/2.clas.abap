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
    METHODS canonical_form
      IMPORTING
        VALUE(iv_word) TYPE string
      RETURNING
        VALUE(rv_canonical_form) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_canonical TYPE string.
    lv_input_canonical = canonical_form( iv_word = input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND
         canonical_form( iv_word = lv_candidate ) = lv_input_canonical.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD canonical_form.
    DATA: lv_word TYPE string.
    " Convert to lower case for case-insensitive comparison
    lv_word = to_lower( iv_word ).
    " Sort characters to create a canonical form
    rv_canonical_form = sort_string( lv_word ).
  ENDMETHOD.
ENDCLASS.
