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
        iv_str        TYPE string
      RETURNING
        VALUE(rv_str) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_normalized TYPE string.

    " Normalize the input string
    lv_input_normalized = normalize_string( input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Check if normalized candidate is an anagram of normalized input
      IF normalize_string( lv_candidate ) = lv_input_normalized.
        CONTINUE. " Skip the same word check
      ENDIF.

      " Check if the candidate is an anagram of the input
      IF sort( lv_input_normalized ) = sort( normalize_string( lv_candidate ) ).
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_string.
    " Convert to lowercase and sort the characters in the string
    rv_str = lv_candidate.
    TRANSLATE rv_str TO LOWER CASE.
    SORT rv_str AS TEXT.
  ENDMETHOD.

ENDCLASS.
