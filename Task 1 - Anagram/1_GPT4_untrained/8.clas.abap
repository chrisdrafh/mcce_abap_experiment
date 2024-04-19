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
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_norm_input TYPE string,
          lt_result TYPE string_table.

    lv_norm_input = normalize_string( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_string( lv_candidate ) = lv_norm_input AND lv_candidate <> input.
        APPEND lv_candidate TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_string.
    " Convert the input string to uppercase and sort its characters
    DATA: lv_sorted TYPE string.

    " Convert to uppercase
    lv_sorted = to_upper( iv_string ).

    " Convert string to table of characters, sort them, then convert back to string
    DATA(lt_chars) = cl_abap_char_utilities=>string_to_char_table( lv_sorted ).
    SORT lt_chars.
    rv_string = cl_abap_char_utilities=>char_table_to_string( lt_chars ).

  ENDMETHOD.

ENDCLASS.
