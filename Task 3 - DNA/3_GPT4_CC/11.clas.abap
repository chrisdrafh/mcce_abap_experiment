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
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'DNA strands must be of the same length'.
    ENDIF.

    DATA: lv_index TYPE i,
          lv_count TYPE i VALUE 0.

    DO strlen( first_strand ) TIMES.
      lv_index = sy-index - 1.
      IF substring( val = first_strand off = lv_index len = 1 ) <>
         substring( val = second_strand off = lv_index len = 1 ).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    result = lv_count.
  ENDMETHOD.

ENDCLASS.
