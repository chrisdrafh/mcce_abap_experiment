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
    " Check if the DNA strands are of the same length
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of the same length'.
    ENDIF.

    " Initialize the count of differences
    DATA(diff_count) = 0.

    " Loop through each character in the strands to count differences
    DO strlen( first_strand ) TIMES.
      " Compare characters at the same position
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    " Return the count of differences
    result = diff_count.
  ENDMETHOD.

ENDCLASS.
