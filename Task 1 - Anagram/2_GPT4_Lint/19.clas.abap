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
        iv_word1      TYPE string
        iv_word2      TYPE string
      RETURNING
        VALUE(rv_is_anagram) TYPE abap_bool.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    LOOP AT candidates INTO DATA(lv_candidate).
      IF is_anagram( iv_word1 = input
                     iv_word2 = lv_candidate ) = abap_true.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    rv_is_anagram = abap_false.

    " Normalize both words to the same case
    DATA(lv_normalized_word1) = to_lower( iv_word1 ).
    DATA(lv_normalized_word2) = to_lower( iv_word2 ).

    " Exit if both words are identical
    IF lv_normalized_word1 = lv_normalized_word2.
      RETURN.
    ENDIF.

    " Sort the characters of both words
    DATA(lv_sorted_word1) TYPE string.
    DATA(lv_sorted_word2) TYPE string.
    lv_sorted_word1 = sort_string( lv_normalized_word1 ).
    lv_sorted_word2 = sort_string( lv_normalized_word2 ).

    " Compare sorted words
    rv_is_anagram = xsdbool( lv_sorted_word1 = lv_sorted_word2 ).
  ENDMETHOD.

  METHOD sort_string.
    " Convert string to character table
    DATA: lt_chars TYPE STANDARD TABLE OF c LENGTH 1,
          lv_char  TYPE c LENGTH 1.
    DO strlen( iv_word ) TIMES.
      lv_char = substring( val = iv_word off = sy-index - 1 len = 1 ).
      INSERT lv_char INTO TABLE lt_chars.
    ENDDO.
    " Sort characters
    SORT lt_chars.
    " Convert character table back to string
    LOOP AT lt_chars INTO lv_char.
      CONCATENATE result lv_char INTO result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
