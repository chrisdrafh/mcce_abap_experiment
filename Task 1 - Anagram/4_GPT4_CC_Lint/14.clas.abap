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
        VALUE(rv_normalized_word) TYPE string.

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    " Normalize the input word for comparison
    lv_normalized_input = normalize_word( input ).

    " Initialize the result table
    CLEAR result.

    " Loop over each candidate word
    LOOP AT candidates INTO DATA(lv_candidate).
      " Normalize the candidate word for comparison
      lv_normalized_candidate = normalize_word( lv_candidate ).

      " Check if normalized candidate is an anagram of the normalized input
      " and ensure it is not the same as the input
      IF lv_normalized_candidate = lv_normalized_input AND lv_candidate <> input.
        " Append the candidate to the result table
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_word.
    " Normalize the word by converting it to lower case and sorting the characters
    rv_normalized_word = lv_word.
    TRANSLATE rv_normalized_word TO LOWER CASE.
    SORT rv_normalized_word BY ( COMPONENTS OF rv_normalized_word ).
  ENDMETHOD.

ENDCLASS.
