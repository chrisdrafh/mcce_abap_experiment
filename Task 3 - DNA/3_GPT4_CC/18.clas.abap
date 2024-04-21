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
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    DATA: lv_length TYPE i.

    lv_length = strlen( first_strand ).

    IF lv_length <> strlen( second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Strands must be of equal length'.
    ENDIF.

    result = 0.

    LOOP AT first_strand INTO DATA(lv_char1) INDEX INTO DATA(lv_index).
      IF lv_char1 <> second_strand+lv_index(1).
        result = result + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
