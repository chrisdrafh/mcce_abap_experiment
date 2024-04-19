CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_best_discount
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(max_discount_price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = get_best_discount( basket ).
  ENDMETHOD.

  METHOD get_best_discount.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          discounts TYPE TABLE OF total,
          index TYPE i,
          sets TYPE i,
          group_size TYPE i.

    FIELD-SYMBOLS: <book_count> TYPE i.

    " Count each book type
    LOOP AT basket ASSIGNING FIELD-SYMBOL(<basket_item>).
      book_counts[ <basket_item> ] = book_counts[ <basket_item> ] + 1.
    ENDLOOP.

    " Calculate discount for each set size from 5 to 1
    DO 5 TIMES.
      group_size = 6 - sy-index.
      sets = lines( book_counts ).

      " Ensure we can actually create a set of this size
      WHILE sets >= group_size.
        " Check we can take one book of each type
        LOOP AT book_counts ASSIGNING <book_count>.
          IF <book_count> < group_size.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF sy-subrc = 0.
          " Discount calculation based on group size
          CASE group_size.
            WHEN 5.
              discounts = discounts + group_size * 8 * 0.75.
            WHEN 4.
              discounts = discounts + group_size * 8 * 0.80.
            WHEN 3.
              discounts = discounts + group_size * 8 * 0.90.
            WHEN 2.
              discounts = discounts + group_size * 8 * 0.95.
            WHEN 1.
              discounts = discounts + group_size * 8.
          ENDCASE.
          " Reduce counts
          LOOP AT book_counts ASSIGNING <book_count>.
            <book_count> = <book_count> - group_size.
          ENDLOOP.
          sets = lines( book_counts ).
        ELSE.
          CONTINUE.
        ENDIF.
      ENDWHILE.
    ENDDO.

    " Calculate the total from the accumulated discounts
    max_discount_price = REDUCE total( INIT x TYPE total FOR <discount> IN discounts NEXT x = x + <discount> ).
  ENDMETHOD.

ENDCLASS.
