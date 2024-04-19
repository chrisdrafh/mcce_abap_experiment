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

  PRIVATE SECTION.
    METHODS check_length
      IMPORTING
        iv_strand1 TYPE string
        iv_strand2 TYPE string
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    " First check if both strings are of the same length
    check_length( iv_strand1 = first_strand iv_strand2 = second_strand ).

    " Initialize distance counter
    DATA(lv_distance) = 0.

    " Loop through each character of the strings
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    " Return the counted differences
    result = lv_distance.
  ENDMETHOD.

  METHOD check_length.
    " Raise exception if lengths differ
    IF strlen( iv_strand1 ) <> strlen( iv_strand2 ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = cx_parameter_invalid=>standard
                  message = 'The lengths of the DNA strands must be equal'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
