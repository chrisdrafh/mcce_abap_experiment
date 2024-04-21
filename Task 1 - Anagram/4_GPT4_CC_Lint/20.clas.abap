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
    METHODS get_canonical_form
      IMPORTING
        word TYPE string
      RETURNING
        VALUE(canonical_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_lower TYPE string,
          lv_candidate_lower TYPE string,
          lv_input_canonical TYPE string.

    lv_input_lower = to_lower( input ).
    lv_input_canonical = get_canonical_form( lv_input_lower ).

    CLEAR result.
    LOOP AT candidates INTO DATA(lv_candidate).
      lv_candidate_lower = to_lower( lv_candidate ).
      IF lv_candidate_lower = lv_input_lower.
        CONTINUE. " Exclude identical words
      ENDIF.

      IF get_canonical_form( lv_candidate_lower ) = lv_input_canonical.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_canonical_form.
    DATA: lv_sorted TYPE string_table.
    FIELD-SYMBOLS: <fs_char> TYPE string.

    SPLIT word AT '' INTO TABLE lv_sorted.
    SORT lv_sorted.
    CONCATENATE LINES OF lv_sorted INTO canonical_word.
  ENDMETHOD.
ENDCLASS.
