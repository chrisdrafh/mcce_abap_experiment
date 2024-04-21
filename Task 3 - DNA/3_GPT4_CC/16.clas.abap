CLASS cx_parameter_invalid DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor IMPORTING textid LIKE cx_static_check=>textid OPTIONAL
                         previous LIKE previous OPTIONAL.
ENDCLASS.

CLASS cx_parameter_invalid IMPLEMENTATION.
  METHOD constructor.
    super->constructor( EXPORTING textid = textid previous = previous ).
  ENDMETHOD.
ENDCLASS.

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
    DATA: lv_length1 TYPE i,
          lv_length2 TYPE i.

    lv_length1 = strlen( first_strand ).
    lv_length2 = strlen( second_strand ).

    " Check if both strands are of equal length
    IF lv_length1 <> lv_length2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = cx_parameter_invalid=>default_text.
    ENDIF.

    " Calculate the Hamming Distance
    DATA: lv_index TYPE i,
          lv_count TYPE i.

    DO lv_length1 TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    result = lv_count.
  ENDMETHOD.
ENDCLASS.
