CLASS zcl_hamming DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
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
          lv_len2 TYPE i.

    lv_len1 = strlen( first_strand ).
    lv_len2 = strlen( second_strand ).

    IF lv_len1 <> lv_len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = if_t100_message=>standard
          msg    = 'DNA strands must be of equal length'.
    ENDIF.

    DATA(lv_distance) = 0.

    DO lv_len1 TIMES.
      DATA(lv_index) = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.
  ENDMETHOD.
ENDCLASS.
