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
        VALUE(rv_sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    DATA: lv_normalized_input TYPE string,
          lv_normalized_candidate TYPE string.

    " Normalize and sort the input word
    lv_normalized_input = normalize_and_sort( input ).

    " Loop through each candidate word
    LOOP AT candidates INTO DATA(lv_candidate).
      " Normalize and sort the candidate word
      lv_normalized_candidate = normalize_and_sort( lv_candidate ).

      " Check if normalized words match and are not the exact same word (case insensitive)
      IF lv_normalized_input = lv_normalized_candidate AND NOT ( CONDENSE( input ) = CONDENSE( lv_candidate ) ).
        APPEND lv_candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_and_sort.
    " Convert to lowercase and remove spaces
    DATA(lv_clean_word) = CONDENSE( to_lower( iv_word ) ).
    DATA(lv_characters) TYPE SORTED TABLE OF char.

    " Split the word into characters and sort them
    DO strlen( lv_clean_word ) TIMES.
      INSERT lv_clean_word+sy-index(1) INTO TABLE lv_characters.
    ENDDO.
    SORT lv_characters.
    CLEAR rv_sorted.
    " Concatenate sorted characters back to a string
    LOOP AT lv_characters INTO DATA(lv_char).
      CONCATENATE rv_sorted lv_char INTO rv_sorted.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
