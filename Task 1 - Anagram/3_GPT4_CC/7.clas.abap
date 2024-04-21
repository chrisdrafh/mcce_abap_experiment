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
    METHODS normalize_and_sort
      IMPORTING
        value         TYPE string
      RETURNING
        VALUE(sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    lv_input_sorted = normalize_and_sort( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_input_sorted = normalize_and_sort( lv_candidate ) AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_and_sort.
    DATA: lv_char_table TYPE STANDARD TABLE OF char.

    " Convert input to lower case and split into a table of characters
    lv_char_table = cl_abap_conv_in_ce=>create( )->convert( cl_abap_conv_in_ce=>uccp( value ) ).
    SORT lv_char_table.
    " Convert sorted characters back into a string
    sorted = REDUCE string( INIT x TYPE string FOR <char> IN lv_char_table NEXT x = x && <char> ).
  ENDMETHOD.
ENDCLASS.
