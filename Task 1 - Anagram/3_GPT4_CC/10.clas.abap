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
        word          TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: normalized_input TYPE string.

    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF normalize_word( candidate ) = normalized_input AND candidate <> input.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    DATA: lt_chars TYPE TABLE OF char.

    " Convert word to lowercase and split into individual characters
    lt_chars = cl_abap_conv_in_ce=>create( )->convert(
      source = to_lower( word )
      options = cl_abap_conv_in_ce=>create_options( byte_mode = abap_false )
    ).

    " Sort the characters to normalize
    SORT lt_chars.
    normalized_word = REDUCE string(
      INIT x TYPE string
      FOR <char> IN lt_chars
      NEXT x = x && <char>
    ).
  ENDMETHOD.
ENDCLASS.
