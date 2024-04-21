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
    DATA: lv_length1 TYPE i,
          lv_length2 TYPE i.

    lv_length1 = strlen( first_strand ).
    lv_length2 = strlen( second_strand ).

    IF lv_length1 <> lv_length2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    DATA(lv_distance) = 0.

    DO lv_length1 TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.
  ENDMETHOD.
ENDCLASS.
