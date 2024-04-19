CLASS zcx_parameter_invalid DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor IMPORTING textid LIKE sy-msgid OPTIONAL
                         iv_message_text TYPE string OPTIONAL.
ENDCLASS.

CLASS zcx_parameter_invalid IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid = textid
                        iv_message_text = iv_message_text ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_hamming DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS hamming_distance
      IMPORTING
        first_strand TYPE string
        second_strand TYPE string
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_parameter_invalid.
ENDCLASS.

CLASS zcl_hamming IMPLEMENTATION.
  METHOD hamming_distance.
    DATA: lv_len1 TYPE i,
          lv_len2 TYPE i,
          lv_diff_count TYPE i.

    lv_len1 = strlen( first_strand ).
    lv_len2 = strlen( second_strand ).

    IF lv_len1 <> lv_len2.
      RAISE EXCEPTION TYPE zcx_parameter_invalid
        EXPORTING
        iv_message_text = 'Strands must be of equal length'.
    ENDIF.

    lv_diff_count = 0.

    DO lv_len1 TIMES.
      IF first_strand+sy-index-1(1) <> second_strand+sy-index-1(1).
        lv_diff_count = lv_diff_count + 1.
      ENDIF.
    ENDDO.

    result = lv_diff_count.
  ENDMETHOD.
ENDCLASS.
