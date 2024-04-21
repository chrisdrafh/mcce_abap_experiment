CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
        iv_string   TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    " Normalize the input string for comparison
    normalized_input = normalize_string( input ).

    " Initialize result table
    CLEAR result.
    
    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Normalize the candidate string
      lv_normalized_candidate = normalize_string( lv_candidate ).

      " Compare normalized strings to check if they are anagrams
      IF normalized_input = lv_normalized_candidate AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_string.
    " Convert to lowercase and sort the characters in the string
    rv_string = lv_string.
    TRANSLATE rv_string TO LOWER CASE.
    SORT rv_string BY ( AS BYTE SEQUENCE ).
  ENDMETHOD.

ENDCLASS.
