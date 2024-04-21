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
          lv_counter TYPE i.

    lv_len1 = strlen( first_strand ).
    lv_len2 = strlen( second_strand ).

    IF lv_len1 <> lv_len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = cx_parameter_invalid=>standard_text
                  message = 'Strands must be of equal length'.
    ENDIF.

    result = 0. " Initialize the result counter

    DO lv_len1 TIMES.
      IF first_strand+lv_counter(1) <> second_strand+lv_counter(1).
        result = result + 1.
      ENDIF.
      lv_counter = lv_counter + 1.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
