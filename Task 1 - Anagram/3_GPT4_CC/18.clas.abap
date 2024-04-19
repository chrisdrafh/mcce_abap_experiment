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
        input    TYPE string
      RETURNING
        VALUE(normalized) TYPE string.

    METHODS are_anagrams
      IMPORTING
        str1 TYPE string
        str2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = normalize_string( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF are_anagrams( normalized_input, normalize_string( candidate ) ) AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    normalized = to_lower( input ). " Convert input to lowercase
    SORT normalized BY ( CONV char( val ) ). " Sort the characters
  ENDMETHOD.

  METHOD are_anagrams.
    is_anagram = xsdbool( str1 = str2 ). " Check for anagram status by comparing sorted strings
  ENDMETHOD.
ENDCLASS.
