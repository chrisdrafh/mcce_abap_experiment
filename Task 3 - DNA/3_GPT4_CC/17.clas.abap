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
    DATA: lv_length_first TYPE i,
          lv_length_second TYPE i.

    lv_length_first = strlen( first_strand ).
    lv_length_second = strlen( second_strand ).

    " Check if strands are of equal length
    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard
          previous = COND #( WHEN lv_length_first = 0 OR lv_length_second = 0 THEN cx_root=>get_last( ) ELSE NULL ).
    ENDIF.

    " Calculate the Hamming Distance
    DATA(lv_distance) = 0.
    DATA(lv_index) TYPE i.

    DO lv_length_first TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.
  ENDMETHOD.

ENDCLASS.
