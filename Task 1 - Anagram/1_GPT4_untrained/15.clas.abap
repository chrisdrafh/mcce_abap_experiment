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
        input1 TYPE string
        input2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    CLEAR result.
    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( input, candidate ) = abap_true AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA(lv_input1_sorted) TYPE string.
    DATA(lv_input2_sorted) TYPE string.

    lv_input1_sorted = to_lower( input1 ).
    SORT lv_input1_sorted BY character.

    lv_input2_sorted = to_lower( input2 ).
    SORT lv_input2_sorted BY character.

    is_anagram = xsdbool( lv_input1_sorted = lv_input2_sorted ).
  ENDMETHOD.

ENDCLASS.
