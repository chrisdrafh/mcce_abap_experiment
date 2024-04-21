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
    METHODS sort_string
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(rv_sorted_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    " Normalize the input string
    DATA lv_input_sorted TYPE string.
    lv_input_sorted = sort_string( iv_string = to_lower( input ) ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      DATA lv_candidate_sorted TYPE string.
      " Normalize and sort the candidate string
      lv_candidate_sorted = sort_string( iv_string = to_lower( lv_candidate ) ).

      " Check if sorted strings match and original strings are not identical
      IF lv_candidate_sorted = lv_input_sorted AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    " Convert the string to a character table, sort it, and convert it back to string
    DATA lt_chars TYPE SORTED TABLE OF char WITH UNIQUE KEY table_line.
    DO strlen( iv_string ) TIMES.
      INSERT iv_string+sy-index(1) INTO TABLE lt_chars.
    ENDDO.
    CLEAR rv_sorted_string.
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE rv_sorted_string lv_char INTO rv_sorted_string.
    ENDLOOP.
  ENDMETHOD.
  
ENDCLASS.
