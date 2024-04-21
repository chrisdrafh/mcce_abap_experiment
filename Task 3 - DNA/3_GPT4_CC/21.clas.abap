CLASS zcl_hamming DEFINITION PUBLIC CREATE PUBLIC.
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
    DATA: len_first TYPE i,
          len_second TYPE i.

    len_first = strlen( first_strand ).
    len_second = strlen( second_strand ).

    IF len_first <> len_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>default_textid
          previous = CONV cx_root( ).
    ENDIF.

    DATA(distance) = 0.
    DO len_first TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        distance = distance + 1.
      ENDIF.
    ENDDO.

    result = distance.
  ENDMETHOD.

ENDCLASS.
