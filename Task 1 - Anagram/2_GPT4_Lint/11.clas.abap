CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sort_char,
             char TYPE c LENGTH 1,
           END OF ty_sort_char,
           tt_sort_char TYPE SORTED TABLE OF ty_sort_char WITH UNIQUE KEY char.

    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      normalize_and_sort
        IMPORTING
          iv_word      TYPE string
        RETURNING
          VALUE(rv_sorted_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string.

    normalized_input = normalize_and_sort( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_and_sort( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_and_sort.
    DATA: lt_chars TYPE tt_sort_char.

    LOOP AT CONV string( to_upper( iv_word ) ) INTO DATA(lv_char).
      APPEND VALUE ty_sort_char( char = lv_char ) TO lt_chars.
    ENDLOOP.

    SORT lt_chars BY char ASCENDING.

    CLEAR rv_sorted_word.
    LOOP AT lt_chars INTO DATA(ls_char).
      CONCATENATE rv_sorted_word ls_char-char INTO rv_sorted_word.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
