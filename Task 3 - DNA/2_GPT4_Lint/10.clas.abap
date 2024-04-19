CLASS zcl_hamming DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS hamming_distance
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS check_length
      IMPORTING
        iv_strand1 TYPE string
        iv_strand2 TYPE string
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    " First, check if both DNA strands are of the same length
    check_length( iv_strand1 = first_strand iv_strand2 = second_strand ).

    " Initialize a counter for the differences
    DATA(lv_diff_count) = 0.

    " Loop over the length of the DNA strands to find differences
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_diff_count = lv_diff_count + 1.
      ENDIF.
    ENDDO.

    " Return the count of differences
    result = lv_diff_count.
  ENDMETHOD.

  METHOD check_length.
    IF strlen( iv_strand1 ) <> strlen( iv_strand2 ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
