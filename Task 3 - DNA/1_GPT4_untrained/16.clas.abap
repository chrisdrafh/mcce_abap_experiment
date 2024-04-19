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
    DATA: lv_length_first  TYPE i,
          lv_length_second TYPE i.

    lv_length_first  = strlen( first_strand ).
    lv_length_second = strlen( second_strand ).

    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA: lv_index    TYPE i,
          lv_hamming  TYPE i VALUE 0.

    DO lv_length_first TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_hamming = lv_hamming + 1.
      ENDIF.
    ENDDO.

    result = lv_hamming.
  ENDMETHOD.

ENDCLASS.
