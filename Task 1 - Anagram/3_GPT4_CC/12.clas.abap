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

  PRIVATE SECTION.
    METHODS normalize_word
      IMPORTING
        word          TYPE string
      RETURNING
        VALUE(sorted_word) TYPE string.

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA(normalized_input) = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalized_input <> normalize_word( candidate ).
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    DATA(lv_word) = to_upper( word ).
    SORT lv_word AS TEXT INTO sorted_word.
  ENDMETHOD.

ENDCLASS.
