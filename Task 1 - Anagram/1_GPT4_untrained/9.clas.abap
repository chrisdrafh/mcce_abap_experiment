CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_word,
             word TYPE string,
             sorted TYPE string,
           END OF ty_word,
           tt_word TYPE TABLE OF ty_word.

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
        iv_word TYPE string
      RETURNING
        VALUE(rv_sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: lv_input_sorted TYPE string,
          lt_words TYPE tt_word,
          ls_word TYPE ty_word.

    " Normalize and sort the input word
    lv_input_sorted = normalize_and_sort( input ).

    " Prepare the candidate words
    LOOP AT candidates INTO DATA(lv_candidate).
      ls_word-word = lv_candidate.
      ls_word-sorted = normalize_and_sort( lv_candidate ).
      APPEND ls_word TO lt_words.
    ENDLOOP.

    " Filter to find anagrams, excluding the identical word
    LOOP AT lt_words INTO ls_word WHERE sorted = lv_input_sorted AND word <> input.
      APPEND ls_word-word TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD normalize_and_sort.
    " Convert to lowercase and sort the characters in the word
    DATA(lv_chars) = cl_abap_converter=>to_lower_case( iv_word ).
    DATA(lt_chars) TYPE SORTED TABLE OF char.
    DO strlen( lv_chars ) TIMES.
      INSERT lv_chars+sy-index(1) INTO TABLE lt_chars.
    ENDDO.
    SORT lt_chars.
    CLEAR rv_sorted.
    LOOP AT lt_chars INTO DATA(lv_char).
      CONCATENATE rv_sorted lv_char INTO rv_sorted.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
