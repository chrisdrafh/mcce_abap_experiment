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
        value TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string,
          normalized_candidate TYPE string.

    " Normalize the input word
    normalized_input = normalize_string( input ).

    " Iterate over each candidate
    LOOP AT candidates INTO DATA(candidate).
      " Normalize candidate string
      normalized_candidate = normalize_string( candidate ).

      " Check if normalized candidate is anagram of the normalized input
      " Exclude exact matches of the original input (case-sensitive check)
      IF normalized_candidate = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    " Convert to uppercase and sort the characters of the string
    result = to_upper( value ).
    SORT result BY ( as string ).
  ENDMETHOD.
ENDCLASS.
