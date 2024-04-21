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
        VALUE(rv_normalized_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string,
          normalized_candidate TYPE string,
          lt_result TYPE string_table.

    normalized_input = normalize_string( iv_string = input ).

    LOOP AT candidates INTO DATA(candidate).
      IF input <> candidate. " Ensure the candidate is not the input itself
        normalized_candidate = normalize_string( iv_string = candidate ).
        IF normalized_input = normalized_candidate.
          APPEND candidate TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_string.
    rv_normalized_string = to_lower( iv_string ).
    SORT rv_normalized_string BY ( COMPONENTS OF rv_normalized_string ). " Sort characters
  ENDMETHOD.
ENDCLASS.
