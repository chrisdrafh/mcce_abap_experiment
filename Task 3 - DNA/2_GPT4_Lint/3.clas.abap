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
    DATA: lv_length_first TYPE i,
          lv_length_second TYPE i.

    lv_length_first  = strlen( first_strand ).
    lv_length_second = strlen( second_strand ).

    " Check if the strands have equal length
    IF lv_length_first <> lv_length_second.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = VALUE cx_parameter_invalid=>textid( area = 'ZHAMMING' 
                                                      id = 'INVALID_LENGTH'
                                                      number = '001' )
          previous = NULL.
    ENDIF.

    " Calculate the Hamming Distance
    DATA(lv_distance) = 0.
    DO lv_length_first TIMES.
      IF first_strand+sy-index(1) <> second_strand+sy-index(1).
        lv_distance = lv_distance + 1.
      ENDIF.
    ENDDO.

    result = lv_distance.
  ENDMETHOD.
ENDCLASS.
