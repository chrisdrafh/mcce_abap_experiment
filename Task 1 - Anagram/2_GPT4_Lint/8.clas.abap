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

  PRIVATE SECTION.
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    CLEAR result.
    DATA: lv_input_lower TYPE string.
    lv_input_lower = to_lower( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF is_anagram( lv_input_lower, to_lower( lv_candidate ) ) AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA: lt_chars1 TYPE SORTED TABLE OF char,
          lt_chars2 TYPE SORTED TABLE OF char.

    " Convert strings to character tables
    lt_chars1 = CAST sorted_table_of_char( VALUE #( FOR c IN word1 ( c ) ) ).
    lt_chars2 = CAST sorted_table_of_char( VALUE #( FOR c IN word2 ( c ) ) ).

    " Compare sorted tables
    is_anagram = xsdbool( lt_chars1 = lt_chars2 ).
  ENDMETHOD.

ENDCLASS.
