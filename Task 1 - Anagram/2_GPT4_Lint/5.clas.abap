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
    METHODS normalize_and_sort
      IMPORTING
        VALUE(iv_word) TYPE string
      RETURNING
        VALUE(rv_normalized) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_input_sorted TYPE string.

    " Normalize and sort the input word
    lv_input_sorted = normalize_and_sort( input ).

    " Loop through each candidate
    LOOP AT candidates INTO DATA(lv_candidate).
      " Check if the candidate is different from the input and if sorted it matches the input
      IF lv_candidate <> input AND normalize_and_sort( lv_candidate ) = lv_input_sorted.
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_and_sort.
    " Convert to lowercase and sort characters of the word
    DATA(lv_chars) TYPE SORTED TABLE OF char.
    FIELD-SYMBOLS(<lv_char>) TYPE char.

    " Normalize by converting to lowercase
    iv_word = to_lower( iv_word ).

    " Split into characters and sort
    DO strlen( iv_word ) TIMES.
      INSERT iv_word+sy-index(1) INTO TABLE lv_chars.
    ENDDO.
    SORT lv_chars.

    " Build the sorted word
    LOOP AT lv_chars ASSIGNING <lv_char>.
      CONCATENATE rv_normalized <lv_char> INTO rv_normalized.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
