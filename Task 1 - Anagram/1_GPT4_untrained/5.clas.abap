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
        word          TYPE string
      RETURNING
        VALUE(sorted) TYPE string.
ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    " Local variables to hold normalized input and candidate words
    DATA: normalized_input TYPE string,
          normalized_candidate TYPE string.
    
    " Normalize the input word
    normalized_input = normalize_word( input ).

    " Clear the result before processing
    CLEAR result.

    " Loop through each candidate word
    LOOP AT candidates INTO DATA(candidate).
      " Normalize and compare each candidate word
      normalized_candidate = normalize_word( candidate );
      IF normalized_input = normalized_candidate AND input <> candidate.
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert the word to uppercase, sort the letters and return the sorted string
    DATA: lt_chars TYPE STANDARD TABLE OF char.
    " Convert to upper case
    word = to_upper( word ).
    " Move to character table
    lt_chars = cl_abap_char_utilities=>string_to_table( word ).
    " Sort characters
    SORT lt_chars.
    " Convert back to string
    sorted = cl_abap_char_utilities=>table_to_string( lt_chars ).
  ENDMETHOD.

ENDCLASS.
