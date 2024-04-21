CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: lt_count TYPE TABLE OF i INDEX TABLE,
          lv_book_id TYPE i,
          lv_total TYPE p LENGTH 3 DECIMALS 2 VALUE 0,
          lv_book_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8,
          lv_group_discount TYPE p LENGTH 5 DECIMALS 2.

    " Count each book in the basket
    LOOP AT basket INTO lv_book_id.
      lt_count[ lv_book_id ] = lt_count[ lv_book_id ] + 1.
    ENDLOOP.

    " Calculate maximum discount by grouping books
    WHILE lines( lt_count ) > 0.
      CASE lines( lt_count ).
        WHEN 5.
          lv_group_discount = lv_book_price * 5 * 0.75. " 25% discount for 5 books
          CLEAR lt_count.
        WHEN 4.
          lv_group_discount = lv_book_price * 4 * 0.80. " 20% discount for 4 books
          REDUCE lt_count. " Reduce each count by 1
        WHEN 3.
          lv_group_discount = lv_book_price * 3 * 0.90. " 10% discount for 3 books
          REDUCE lt_count. " Reduce each count by 1
        WHEN 2.
          lv_group_discount = lv_book_price * 2 * 0.95. " 5% discount for 2 books
          REDUCE lt_count. " Reduce each count by 1
        WHEN 1.
          lv_group_discount = lv_book_price * 1. " No discount for 1 book
          REDUCE lt_count. " Reduce each count by 1
        WHEN OTHERS.
          lv_group_discount = lv_book_price * lines( lt_count ). " No grouping possible
          CLEAR lt_count.
      ENDCASE.

      lv_total += lv_group_discount.
    ENDWHILE.

    total = lv_total.
  ENDMETHOD.

ENDCLASS.
