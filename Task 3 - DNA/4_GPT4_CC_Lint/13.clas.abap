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
          lv_len_second TYPE i,
          lv_counter TYPE i.

    lv_len_first = strlen( first_strand ).
    lv_len_second = strlen( second_strand ).

    " Check if both strings are of equal length
    IF lv_len_first <> lv_len_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'DNA strands must be of equal length.'.
    ENDIF.

    " Initialize the result
    result = 0.

    " Loop through each character and compare
    DO lv_len_first TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        result = result + 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
