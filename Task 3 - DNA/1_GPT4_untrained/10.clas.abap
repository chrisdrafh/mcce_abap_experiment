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
    DATA: lv_length TYPE i.

    " Check if both strands are of equal length
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Initialize result
    result = 0.
    lv_length = strlen( first_strand ).

    " Calculate Hamming distance
    DO lv_length TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
