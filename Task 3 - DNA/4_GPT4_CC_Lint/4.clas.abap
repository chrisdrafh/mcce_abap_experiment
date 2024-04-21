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

    " Check if strands are of the same length
    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'DNA strands must be of the same length'.
    ENDIF.

    " Calculate Hamming Distance
    DATA(lv_index) = 1.
    DATA(lv_distance) = 0.

    WHILE lv_index <= lv_length_first.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
      lv_index = lv_index + 1.
    ENDWHILE.

    result = lv_distance.
  ENDMETHOD.

ENDCLASS.
