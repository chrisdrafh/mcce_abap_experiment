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
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = to_lower( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( normalized_input, to_lower( candidate ) ) = abap_true AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA: lv_sort1 TYPE string,
          lv_sort2 TYPE string.

    lv_sort1 = sort_chars( word1 ).
    lv_sort2 = sort_chars( word2 ).

    IF lv_sort1 = lv_sort2.
      is_anagram = abap_true.
    ELSE.
      is_anagram = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD sort_chars.
    DATA: lt_chars TYPE TABLE OF char.
    FIELD-SYMBOLS: <char> LIKE LINE OF lt_chars.

    SPLIT word AT '' INTO TABLE lt_chars.
    SORT lt_chars.
    CLEAR word.
    LOOP AT lt_chars ASSIGNING <char>.
      CONCATENATE word <char> INTO word.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
