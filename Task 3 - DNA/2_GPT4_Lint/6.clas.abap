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
    DATA: len1 TYPE i,
          len2 TYPE i,
          i    TYPE i,
          diff_count TYPE i.

    len1 = strlen( first_strand ).
    len2 = strlen( second_strand ).

    IF len1 <> len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Strands must be of equal length'.
    ENDIF.

    diff_count = 0.
    DO len1 TIMES.
      i = sy-index - 1.
      IF first_strand+i(1) <> second_strand+i(1).
        diff_count = diff_count + 1.
      ENDIF.
    ENDDO.

    result = diff_count.

  ENDMETHOD.

ENDCLASS.
