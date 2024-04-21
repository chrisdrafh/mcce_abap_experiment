CLASS zcl_hamming DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS hamming_distance
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.
  METHOD hamming_distance.
    " Check for equal length of strands
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard_text
          previous = CONV cx_root( 'DNA strands must be of equal length' ).
    ENDIF.

    " Initialize a counter for differences
    DATA(diff_count) = 0.

    " Loop through each character in the strands and compare
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    " Return the number of differences
    result = diff_count.
  ENDMETHOD.
ENDCLASS.
