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

ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_sorted TYPE string.
    DATA: lv_candidate_sorted TYPE string.
    FIELD-SYMBOLS: <fs_candidate> TYPE string.

    " Normalize and sort the input word
    lv_input_sorted = to_lower( input ).
    SORT lv_input_sorted BY character.

    " Loop over each candidate word
    LOOP AT candidates ASSIGNING <fs_candidate>.
      DATA(lv_candidate) = <fs_candidate>.
      
      " Skip if the candidate is exactly the input
      IF lv_candidate = input.
        CONTINUE.
      ENDIF.
      
      " Normalize and sort the candidate word
      lv_candidate_sorted = to_lower( lv_candidate ).
      SORT lv_candidate_sorted BY character.

      " Check if sorted candidate matches sorted input (and not the original input)
      IF lv_candidate_sorted = lv_input_sorted AND lv_candidate <> input.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
