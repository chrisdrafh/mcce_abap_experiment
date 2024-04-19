CLASS zcl_hamming DEFINITION PUBLIC.
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
    " Declare local variables
    DATA: len1 TYPE i,
          len2 TYPE i.

    " Calculate the length of each string
    len1 = strlen( first_strand ).
    len2 = strlen( second_strand ).

    " Check if the lengths are equal
    IF len1 <> len2.
      " Raise an exception if the DNA strands are not of the same length
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    " Initialize result
    result = 0.

    " Loop through the strands to count differences
    DO len1 TIMES.
      " Compare characters from each strand
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        " Increment the result for each difference
        result = result + 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
