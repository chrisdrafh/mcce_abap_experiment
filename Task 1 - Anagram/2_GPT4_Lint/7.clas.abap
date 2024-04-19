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
        VALUE(word)   TYPE string
      RETURNING
        VALUE(normalized) TYPE string.

    METHODS is_anagram
      IMPORTING
        VALUE(word1)  TYPE string
        VALUE(word2)  TYPE string
      RETURNING
        VALUE(is_anagram) TYPE abap_bool.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA(normalized_input) TYPE string.
    normalized_input = normalize_word( input ).

    LOOP AT candidates INTO DATA(candidate).
      IF candidate <> input AND is_anagram( normalized_input, candidate ).
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    normalized = to_lower( word ).
  ENDMETHOD.

  METHOD is_anagram.
    is_anagram = abap_false.
    IF normalize_word( word1 ) = normalize_word( word2 ).
      RETURN.
    ENDIF.

    DATA(lt_char_count1) TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA(lt_char_count2) TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

    " Count character occurrences in word1
    DO strlen( word1 ) TIMES.
      DATA(char1) = word1( sy-index ).
      IF char1 IS NOT INITIAL.
        ADD 1 TO lt_char_count1[ char1 ].
      ENDIF.
    ENDDO.

    " Count character occurrences in word2
    DO strlen( word2 ) TIMES.
      DATA(char2) = word2( sy-index ).
      IF char2 IS NOT INITIAL.
        ADD 1 TO lt_char_count2[ char2 ].
      ENDIF.
    ENDDO.

    " Compare the tables
    IF lt_char_count1 = lt_char_count2.
      is_anagram = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
