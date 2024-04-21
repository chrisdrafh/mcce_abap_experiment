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

ENDCLASS.

CLASS zcl_anagram IMPLEMENTATION.

  METHOD anagram.
    DATA: normalized_input TYPE string.
    DATA: sorted_input TYPE string.
    DATA: sorted_candidate TYPE string.
    
    " Normalize and sort the input
    normalized_input = to_lower( input ).
    CALL METHOD sort_string
      EXPORTING
        unsorted = normalized_input
      IMPORTING
        sorted = sorted_input.
    
    " Loop through candidates to find anagrams
    LOOP AT candidates INTO DATA(candidate).
      DATA: normalized_candidate TYPE string.
      normalized_candidate = to_lower( candidate ).
      
      " Check if the candidate is not exactly the same as the input
      IF normalized_candidate <> normalized_input.
        " Sort the candidate string
        CALL METHOD sort_string
          EXPORTING
            unsorted = normalized_candidate
          IMPORTING
            sorted = sorted_candidate.
        
        " If sorted strings are equal, it's an anagram
        IF sorted_input = sorted_candidate.
          APPEND candidate TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_string.
    DATA: characters TYPE string_table.
    FIELD-SYMBOLS: <char> TYPE string.

    " Split input into characters and sort them
    DO strlen( unsorted ) TIMES.
      APPEND unsorted+sy-index(1) TO characters.
    ENDDO.
    SORT characters.

    " Concatenate back to a sorted string
    LOOP AT characters ASSIGNING <char>.
      CONCATENATE sorted <char> INTO sorted.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
