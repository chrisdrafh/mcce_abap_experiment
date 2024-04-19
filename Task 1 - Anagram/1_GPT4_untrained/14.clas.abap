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
        iv_word       TYPE string
      RETURNING
        VALUE(rv_word) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    " Normalize the input word
    DATA lv_input_norm TYPE string.
    lv_input_norm = normalize_word( input ).

    " Loop through the candidates and find anagrams
    LOOP AT candidates INTO DATA(lv_candidate).
      IF normalize_word( lv_candidate ) = lv_input_norm AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Normalize by converting to lower case and sorting the characters
    DATA: lt_chars TYPE TABLE OF c LENGTH 1.

    " Convert the string to lowercase and split into characters
    lv_word = to_lower( iv_word ).
    SPLIT lv_word AT '' INTO TABLE lt_chars.

    " Sort the characters
    SORT lt_chars.

    " Concatenate the sorted characters back to string
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE rv_word lv_char
