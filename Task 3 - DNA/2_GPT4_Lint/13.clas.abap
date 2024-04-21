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
    DATA: lv_length_first  TYPE i,
          lv_length_second TYPE i.

    " Determine the lengths of both DNA strands
    lv_length_first  = strlen( first_strand ).
    lv_length_second = strlen( second_strand ).

    " Check if the DNA strands are of the same length
    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'DNA strands must be of the same length'.
    ENDIF.

    " Initialize the result
    result = 0.

    " Calculate the Hamming Distance
    LOOP AT first_strand ASSIGNING FIELD-SYMBOL(<fs_first>) INDEX INTO DATA(lv_index).
      IF <fs_first> <> second_strand+lv_index(1).
        result = result + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
