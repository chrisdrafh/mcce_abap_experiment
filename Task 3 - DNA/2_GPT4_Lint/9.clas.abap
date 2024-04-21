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
    " Check if strands are of equal length
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    " Initialize the result counter
    DATA(result) = 0.

    " Iterate over the length of the strands and compare each character
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
