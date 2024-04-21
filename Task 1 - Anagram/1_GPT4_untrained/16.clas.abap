CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
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
    DATA: lv_input TYPE string,
          lv_candidate TYPE string.

    CLEAR result.
    lv_input = to_lower( input ).

    LOOP AT candidates INTO lv_candidate.
      IF is_anagram( lv_input, to_lower( lv_candidate ) ) = abap_true AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA: lv_sorted1 TYPE string,
          lv_sorted2 TYPE string.

    lv_sorted1 = sort_string( word1 ).
    lv_sorted2 = sort_string( word2 ).

    is_anagram = xsdbool( lv_sorted1 = lv_sorted2 AND word1 <> word2 ).
  ENDMETHOD.

  METHOD sort_string.
    DATA: lv_string TYPE string,
          lt_chars TYPE TABLE OF char,
          lv_char TYPE char.

    lv_string = word.
    DO strlen( lv_string ) TIMES.
      lv_char = lv_string+sy-index(1).
      INSERT lv_char INTO TABLE lt_chars.
    ENDDO.

    SORT lt_chars.
    CLEAR word.
    LOOP AT lt_chars INTO lv_char.
      CONCATENATE word lv_char INTO word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
