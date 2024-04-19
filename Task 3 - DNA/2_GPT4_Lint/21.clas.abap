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
    DATA: lv_length1 TYPE i,
          lv_length2 TYPE i,
          lv_index   TYPE i,
          lv_count   TYPE i.

    lv_length1 = strlen( first_strand ).
    lv_length2 = strlen( second_strand ).

    " Check if strands are of equal length
    IF lv_length1 <> lv_length2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = cx_parameter_invalid=>default_textid
          previous = sy-msgid.
    ENDIF.

    " Initialize the counter for differences
    lv_count = 0.

    " Loop through each character in the strings
    DO lv_length1 TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    " Return the count of differences
    result = lv_count.

  ENDMETHOD.

ENDCLASS.
