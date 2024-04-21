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
    DATA: len1 TYPE i VALUE strlen( first_strand ),
          len2 TYPE i VALUE strlen( second_strand ).

    IF len1 <> len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard
          previous = NULL.
    ENDIF.

    DATA(count) = 0.

    DO len1 TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        count = count + 1.
      ENDIF.
    ENDDO.

    result = count.
  ENDMETHOD.

ENDCLASS.
