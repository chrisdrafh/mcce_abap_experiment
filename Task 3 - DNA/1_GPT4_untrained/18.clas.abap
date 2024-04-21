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
    DATA: lv_length1 TYPE i,
          lv_length2 TYPE i.

    lv_length1 = strlen( first_strand ).
    lv_length2 = strlen( second_strand ).

    IF lv_length1 <> lv_length2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    DATA: lv_diff_count TYPE i VALUE 0.

    DO lv_length1 TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_diff_count = lv_diff_count + 1.
      ENDIF.
    ENDDO.

    result = lv_diff_count.
  ENDMETHOD.

ENDCLASS.
