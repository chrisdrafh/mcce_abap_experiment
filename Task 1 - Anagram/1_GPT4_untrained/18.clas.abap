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
    METHODS normalize_word
      IMPORTING
        word          TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.

    METHODS compare_words
      IMPORTING
        word1         TYPE string
        word2         TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.
ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF compare_words( normalized_input, normalize_word( candidate ) ) = abap_true AND
         input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    normalized_word = word.
    " Convert to lower case to handle case insensitivity
    normalized_word = to_lower( normalized_word ).
    " Sort characters
    SORT normalized_word BY ( substring = normalized_word ).
  ENDMETHOD.

  METHOD compare_words.
    IF word1 = word2.
      is_anagram = abap_true.
    ELSE.
      is_anagram = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
