CLASS zcl_hamming DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS: hamming_distance
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    CLASS-METHODS: validate_parameters
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    " Validate that strands are of equal length
    validate_parameters( first_strand  = first_strand
                         second_strand = second_strand ).

    " Initialize the result
    DATA(result) = 0.

    " Calculate the Hamming distance
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.

    RETURN result.
  ENDMETHOD.

  METHOD validate_parameters.
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
        text = 'Strands must be of equal length'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
