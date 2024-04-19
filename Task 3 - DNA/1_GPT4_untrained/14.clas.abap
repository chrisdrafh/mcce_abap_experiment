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
    " Initialize the result
    DATA: result TYPE i VALUE 0.

    " Check if the strands are of the same length
    IF strlen( first_strand ) NE strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of the same length'.
    ENDIF.

    " Calculate the Hamming distance
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) NE second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.

    RETURN.
  ENDMETHOD.

ENDCLASS.
