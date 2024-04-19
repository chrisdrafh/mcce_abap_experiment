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
    " Local variable to count differences
    DATA: lv_count TYPE i VALUE 0.

    " Check if the lengths of the two strands are the same
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Strands must be of equal length'.
    ENDIF.

    " Loop over each character in the strings
    DO strlen( first_strand ) TIMES.
      " Compare characters at the same position
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        ADD 1 TO lv_count.
      ENDIF.
    ENDDO.

    " Return the count of differences
    result = lv_count.

  ENDMETHOD.

ENDCLASS.
