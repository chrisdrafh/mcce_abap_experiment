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
    TYPES: tt_chars TYPE STANDARD TABLE OF char WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.

  METHOD hamming_distance.
    DATA: lt_first_strand TYPE tt_chars,
          lt_second_strand TYPE tt_chars.

    " Convert strings to character tables
    lt_first_strand = cl_abap_conv_in_ce=>create( )->convert( first_strand ).
    lt_second_strand = cl_abap_conv_in_ce=>create( )->convert( second_strand ).

    " Check if strands are of equal length
    IF lines( lt_first_strand ) <> lines( lt_second_strand ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Strands must be of equal length.'.
    ENDIF.

    " Calculate Hamming distance
    result = REDUCE i( INIT x = 0 FOR index = 1 WHILE index <= lines( lt_first_strand ) 
                         NEXT x = x + COND #( WHEN lt_first_strand[ index ] <> lt_second_strand[ index ] THEN 1 ELSE 0 ) ).

  ENDMETHOD.

ENDCLASS.
