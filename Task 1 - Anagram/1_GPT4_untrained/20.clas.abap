CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sort_char,
             char TYPE c LENGTH 1,
           END OF ty_sort_char,
           tt_sort_char TYPE TABLE OF ty_sort_char.

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
        VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    lv_input_sorted = sort_string( input ).

    CLEAR result.
    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND sort_string( lv_candidate ) = lv_input_sorted.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    DATA: lt_char_table TYPE tt_sort_char,
          lv_char       TYPE ty_sort_char.

    DO strlen( iv_string ) TIMES.
      lv_char-char = iv_string+sy-index(1).
      APPEND lv_char TO lt_char_table.
    ENDDO.

    SORT lt_char_table BY char ASCENDING.
    CLEAR rv_string.
    LOOP AT lt_char_table INTO lv_char.
      CONCATENATE rv_string lv_char-char INTO rv_string.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
