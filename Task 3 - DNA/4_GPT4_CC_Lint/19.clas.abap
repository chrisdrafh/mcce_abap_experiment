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
    " Check if strands have the same length
    IF strlen( first_strand ) <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>standard
          previous = cx_root=>get_instance( )
          message = |Strands must be of the same length|.
    ENDIF.

    " Initialize counter for differences
    DATA(differences) = 0.

    " Calculate the Hamming distance
    DO strlen( first_strand ) TIMES.
      " Get position index
      DATA(index) = sy-index - 1.

      " Compare characters at the same position
      IF first_strand+index(1) <> second_strand+index(1).
        differences = differences + 1.
      ENDIF.
    ENDDO.

    " Return the number of differences
    result = differences.
  ENDMETHOD.

ENDCLASS.
