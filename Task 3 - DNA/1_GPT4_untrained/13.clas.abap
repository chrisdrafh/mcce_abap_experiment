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
    DATA: len TYPE i.

    len := strlen( first_strand ).

    " Check if the DNA strands are of the same length
    IF len <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'DNA strands must be of equal length.'.
    ENDIF.

    " Calculate Hamming distance
    DATA(diff_count) = 0.
    DO len TIMES.
      DATA(index) = sy-index - 1.
      IF first_strand+index(1) <> second_strand+index(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    result = diff_count.
  ENDMETHOD.

ENDCLASS.
