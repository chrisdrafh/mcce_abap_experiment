CLASS zcl_hamming DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: hamming_distance
      IMPORTING
        first_strand  TYPE string
        second_strand TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: validate_inputs
      IMPORTING
        iv_strand1 TYPE string
        iv_strand2 TYPE string
      RAISING
        cx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    " Validate inputs: strands must be of the same length
    validate_inputs( iv_strand1 = first_strand iv_strand2 = second_strand ).

    " Initialize a counter for differences
    DATA: lv_diff_count TYPE i VALUE 0.

    " Loop over the length of the strands and compare each character
    DO strlen( first_strand ) TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_diff_count = lv_diff_count + 1.
      ENDIF.
    ENDDO.

    " Return the count of differences
    result = lv_diff_count.
  ENDMETHOD.

  METHOD validate_inputs.
    " Check if both DNA strands are of the same length
    IF strlen( iv_strand1 ) <> strlen( iv_strand2 ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>invalid_input
          previous = cx_root=>get_last_exception( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
