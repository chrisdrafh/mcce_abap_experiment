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
    IF lv_length <> strlen( second_strand ) OR lv_length = 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard_message.
    ENDIF.

    DATA: lv_diff_count TYPE i VALUE 0.
    DO lv_length TIMES.
      DATA(lv_index) = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_diff_count = lv_diff_count + 1.
      ENDIF.
    ENDDO.

    result = lv_diff_count.
  ENDMETHOD.
ENDCLASS.
