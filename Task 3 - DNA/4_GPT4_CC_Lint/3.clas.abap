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
        EXPORTING
          text = 'DNA strands must be of equal length'.
    ENDIF.

    DATA: lv_distance TYPE i VALUE 0.

    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.
  ENDMETHOD.

ENDCLASS.
