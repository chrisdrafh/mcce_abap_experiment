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
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Strands must be of equal length'.
    ENDIF.

    DATA(diff_count) = 0.
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    result = diff_count.
  ENDMETHOD.
ENDCLASS.
