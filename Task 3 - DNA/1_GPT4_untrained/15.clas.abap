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
    " Initialize the counter for the differences
    DATA: lv_count TYPE i VALUE 0.

    " Check if strands are of the same length
    IF strlen( first_strand ) <> strlen( second_strand ).
      " Raise an exception if the lengths are different
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of the same length'.
    ENDIF.

    " Loop over each character in the strands
    DO strlen( first_strand ) TIMES.
      " Compare characters at the same position
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        " Increment the counter if they are different
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    " Return the number of differences
    result = lv_count.

  ENDMETHOD.

ENDCLASS.
