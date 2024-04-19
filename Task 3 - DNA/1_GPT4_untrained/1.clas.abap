CLASS cx_parameter_invalid DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor IMPORTING textid LIKE sy-msgid OPTIONAL
                        previous LIKE previous OPTIONAL.
ENDCLASS.

CLASS cx_parameter_invalid IMPLEMENTATION.
  METHOD constructor.
    SUPER->constructor( EXPORTING textid = textid
                                    previous = previous ).
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
    DATA: lv_len1 TYPE i,
          lv_len2 TYPE i,
          lv_index TYPE i,
          lv_count TYPE i.

    lv_len1 = strlen( first_strand ).
    lv_len2 = strlen( second_strand ).

    IF lv_len1 <> lv_len2.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          textid = 'STRAND_LENGTH_MISMATCH'.
    ENDIF.

    lv_count = 0.
    DO lv_len1 TIMES.
      lv_index = sy-index - 1.
      IF first_strand+lv_index(1) <> second_strand+lv_index(1).
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    result = lv_count.
  ENDMETHOD.
ENDCLASS.
