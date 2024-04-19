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
    " Local variables to store lengths of the strands
    DATA: len1 TYPE i,
          len2 TYPE i.

    " Get the lengths of both strands
    len1 = strlen( first_strand ).
    len2 = strlen( second_strand ).

    " Check if strands are of equal length
    IF len1 <> len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    " Variable to count differences
    DATA differences TYPE i.
    differences = 0.

    " Loop through each character in the strands and compare
    DO len1 TIMES.
      " Compare characters at the same position
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        differences = differences + 1.
      ENDIF.
    ENDDO.

    " Return the number of differences
    result = differences.
  ENDMETHOD.

ENDCLASS.
