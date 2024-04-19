CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_candidate,
             word TYPE string,
           END OF ty_candidate,
           tt_candidates TYPE STANDARD TABLE OF ty_candidate WITH DEFAULT KEY.

    METHODS anagram
      IMPORTING
        input         TYPE string
        candidates    TYPE tt_candidates
      RETURNING
        VALUE(result) TYPE tt_candidates.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anag) TYPE abap_bool.
ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    CLEAR result.
    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( input, candidate-word ).
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    is_anag = abap_false.

    DATA(lv_word1) = to_lower( word1 ).
    DATA(lv_word2) = to_lower( word2 ).

    " A word is not its own anagram
    IF lv_word1 = lv_word2.
      RETURN.
    ENDIF.

    " Sort characters
    SORT VALUE #( FOR <char> IN lv_word1 ( <char> ) ) INTO lv_word1.
    SORT VALUE #( FOR <char> IN lv_word2 ( <char> ) ) INTO lv_word2.

    IF lv_word1 = lv_word2.
      is_anag = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
