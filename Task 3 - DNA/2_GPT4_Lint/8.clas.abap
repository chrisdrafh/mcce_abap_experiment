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
    DATA: lv_length TYPE i.

    lv_length = strlen( first_strand ).

    " Check if the strands are of equal length
    IF lv_length <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>default_textid
          previous = COND #( WHEN first_strand IS INITIAL OR second_strand IS INITIAL THEN 1 ELSE 2 ).
    ENDIF.

    " Calculate the Hamming Distance
    DO lv_length TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
