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
        iv_string     TYPE string
      RETURNING
        VALUE(rv_string) TYPE string.

    METHODS count_characters
      IMPORTING
        iv_string     TYPE string
      RETURNING
        VALUE(rt_counts) TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lt_normalized_candidates TYPE string_table,
          ls_count_input TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
          ls_count_candidate TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
          lv_candidate TYPE string.

    lv_normalized_input = normalize_string( iv_string = input ).
    ls_count_input = count_characters( iv_string = lv_normalized_input ).

    LOOP AT candidates INTO lv_candidate.
      IF normalize_string( iv_string = lv_candidate ) = lv_normalized_input AND lv_candidate <> input.
        CONTINUE.
      ENDIF.

      CLEAR ls_count_candidate.
      ls_count_candidate = count_characters( iv_string = normalize_string( iv_string = lv_candidate ) ).

      IF ls_count_candidate = ls_count_input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_string.
    rv_string = to_lower( iv_string ).
  ENDMETHOD.

  METHOD count_characters.
    DATA: lv_char TYPE c LENGTH 1.

    LOOP AT iv_string INTO lv_char.
      ADD 1 TO rt_counts[ lv_char ].
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
