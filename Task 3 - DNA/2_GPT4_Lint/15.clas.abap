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
    DATA: lv_len_first TYPE i,
          lv_len_second TYPE i.

    lv_len_first = strlen( first_strand ).
    lv_len_second = strlen( second_strand ).

    IF lv_len_first <> lv_len_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    DATA(lv_count) = 0.

    DO lv_len_first TIMES.
      VARIABLE: lv_index TYPE i.
      lv_index = sy-index - 1.

      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    result = lv_count.
  ENDMETHOD.

ENDCLASS.
