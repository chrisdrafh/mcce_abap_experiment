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

  PRIVATE SECTION.
    METHODS check_strands_length
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    DATA: idx TYPE i,
          diff_count TYPE i VALUE 0.

    check_strands_length( first_strand, second_strand ).

    DO strlen( first_strand ) TIMES.
      idx = sy-index - 1.
      IF first_strand+idx(1) <> second_strand+idx(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    result = diff_count.
  ENDMETHOD.

  METHOD check_strands_length.
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>default_text
          previous = NULL.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
