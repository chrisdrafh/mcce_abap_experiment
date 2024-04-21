CLASS zcl_hamming DEFINITION PUBLIC FINAL CREATE PUBLIC.
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
    DATA: len1 TYPE i,
          len2 TYPE i.

    len1 = strlen( first_strand ).
    len2 = strlen( second_strand ).

    IF len1 <> len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard_message
          previous = COND #( WHEN len1 = 0 OR len2 = 0 THEN 'Either string is empty.'
                             ELSE 'Strings are of unequal length.' ).
    ENDIF.

    result = 0.
    DO len1 TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
