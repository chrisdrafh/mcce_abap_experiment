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
    DATA: lv_length_first TYPE i,
          lv_length_second TYPE i.

    lv_length_first = strlen( first_strand ).
    lv_length_second = strlen( second_strand ).

    " Check if the DNA strands are of equal length
    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard_message
          previous = sy-subrc.
    ENDIF.

    " Calculate Hamming Distance
    DATA: lv_distance TYPE i VALUE 0.
    DO lv_length_first TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.

  ENDMETHOD.

ENDCLASS.
