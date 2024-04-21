CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_max_discount_set
      IMPORTING 
        io_basket TYPE REF TO basket_type
      RETURNING VALUE(ro_set) TYPE basket_type.

    METHODS calculate_set_price
      IMPORTING 
        is_set TYPE basket_type
      RETURNING VALUE(rv_price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: lv_total TYPE total,
          lo_basket TYPE REF TO basket_type,
          lo_set TYPE basket_type.

    lo_basket = NEW basket_type( ).
    lo_basket = basket.

    WHILE lo_basket IS NOT INITIAL.
      lo_set = get_max_discount_set( lo_basket ).
      lv_total += calculate_set_price( lo_set ).
    ENDWHILE.

    total = lv_total.
  ENDMETHOD.

  METHOD get_max_discount_set.
    DATA: lt_count TYPE TABLE OF i INDEX TABLE,
          lt_set TYPE basket_type,
          lv_book TYPE book_id.

    " Count books
    LOOP AT io_basket INTO lv_book.
      ADD 1 TO lt_count[ lv_book ].
    ENDLOOP.

    " Form the largest set possible
    DO 5 TIMES.
      LOOP AT lt_count INTO lv_book WHERE table_line > 0.
        APPEND lv_book TO lt_set.
        SUBTRACT 1 FROM lt_count[ lv_book ].
      ENDLOOP.
      IF lines( lt_set ) >= 3.
        EXIT.
      ELSE.
        CLEAR lt_set.
      ENDIF.
    ENDDO.

    " Update original basket
    LOOP AT lt_set INTO lv_book.
      DELETE io_basket FROM TABLE INDEX sy-tabix.
    ENDLOOP.

    ro_set = lt_set.
  ENDMETHOD.

  METHOD calculate_set_price.
    CASE lines( is_set ).
      WHEN 5.
        rv_price = 5 * 8 * ( 1 - 0.25 ).
      WHEN 4.
        rv_price = 4 * 8 * ( 1 - 0.20 ).
      WHEN 3.
        rv_price = 3 * 8 * ( 1 - 0.10 ).
      WHEN 2.
        rv_price = 2 * 8 * ( 1 - 0.05 ).
      WHEN OTHERS.
        rv_price = lines( is_set ) * 8.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
