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
        VALUE(rv_normalized) TYPE string.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string.

    " Normalize the input word
    lv_normalized_input = normalize_word( input ).

    " Loop through the candidates and select the anagrams
    LOOP AT candidates INTO DATA(lv_candidate).
      IF lv_normalized_input = normalize_word( lv_candidate ) AND input <> lv_candidate.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Normalize a word by converting it to lowercase and sorting its characters
    rv_normalized = lv_word.
    TRANSLATE rv_normalized TO LOWER CASE.
    SORT rv_normalized AS TEXT INTO rv_normalized.
  ENDMETHOD.

ENDCLASS.
