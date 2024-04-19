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
        VALUE(is_an) TYPE abap_bool.
ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    " Convert the input to uppercase for comparison purposes
    DATA lv_input_upper TYPE string.
    lv_input_upper = to_upper( input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Check if candidate is an anagram and is not exactly the input word
      IF is_anagram( lv_input_upper, to_upper( lv_candidate ) ) AND lv_candidate <> input.
        " Append original candidate to the result
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    " Check if both words are anagrams by comparing their sorted characters
    DATA(lv_word1_sorted) = lv_word1.
    DATA(lv_word2_sorted) = lv_word2.

    SORT lv_word1_sorted BY COMPONENTS.
    SORT lv_word2_sorted BY COMPONENTS.

    " Compare sorted strings
    is_an = xsdbool( lv_word1_sorted = lv_word2_sorted ).
  ENDMETHOD.

ENDCLASS.
