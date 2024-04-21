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
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    LOOP AT candidates INTO DATA(candidate).
      IF candidate <> input AND is_anagram( input, candidate ) = abap_true.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    DATA: normalized_word1 TYPE string,
          normalized_word2 TYPE string.

    normalized_word1 = to_upper( word1 ).
    normalized_word2 = to_upper( word2 ).

    SORT VALUE #( FOR i = 1 THEN i + 1 WHILE i <= strlen( normalized_word1 )
                  ( substring( val = normalized_word1 off = i - 1 len = 1 ) ) ) INTO normalized_word1.
    SORT VALUE #( FOR i = 1 THEN i + 1 WHILE i <= strlen( normalized_word2 )
                  ( substring( val = normalized_word2 off = i - 1 len = 1 ) ) ) INTO normalized_word2.

    is_anagram = xsdbool( normalized_word1 = normalized_word2 ).
  ENDMETHOD.

ENDCLASS.
