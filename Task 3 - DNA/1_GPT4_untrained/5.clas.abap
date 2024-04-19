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
    DATA: lv_len1 TYPE i,
          lv_len2 TYPE i,
          lv_index TYPE i,
          lv_count TYPE i.

    lv_len1 = strlen( first_strand ).
    lv_len2 = strlen( second_strand ).

    " Check if both DNA strands are of equal length
    IF lv_len1 <> lv_len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>default_text
          previous = NULL
          message = 'DNA strands must be of equal length'.
    ENDIF.

    " Calculate the Hamming Distance
    lv_count = 0.
    DO lv_len1 TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    result = lv_count.

  ENDMETHOD.

ENDCLASS.
