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
    METHODS normalize_word
      IMPORTING
        VALUE(word) TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lt_result TYPE string_table.

    lv_normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_candidate <> input AND normalize_word( lv_candidate ) = lv_normalized_input.
        APPEND lv_candidate TO lt_result.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_word.
    normalized_word = to_lower( word ). " Convert to lower case to ensure case insensitivity
    SORT normalized_word BY character ASCENDING. " Sort characters in alphabetical order
  ENDMETHOD.

ENDCLASS.
