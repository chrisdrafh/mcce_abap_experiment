CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount_price) TYPE p.

    METHODS maximize_discount
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(price) TYPE p.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = maximize_discount( basket ).
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 2.
        discount_price = 2 * 8 * 0.95.
      WHEN 3.
        discount_price = 3 * 8 * 0.90.
      WHEN 4.
        discount_price = 4 * 8 * 0.80.
      WHEN 5.
        discount_price = 5 * 8 * 0.75.
      WHEN OTHERS.
        discount_price = num_books * 8.
    ENDCASE.
  ENDMETHOD.

  METHOD maximize_discount.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          max_books TYPE i VALUE 5,
          group_size TYPE i,
          tmp_basket TYPE basket_type.

    FIELD-SYMBOLS: <book> TYPE book_id.

    LOOP AT basket INTO <book>.
      counts[ <book> ] = counts[ <book> ] + 1.
    ENDLOOP.

    DO max_books TIMES.
      CLEAR tmp_basket.
      group_size = 0.

      " Try to create the largest set of different books
      LOOP AT counts INTO DATA(count) FROM max_books TO 1 WHERE count > 0.
        APPEND count TO tmp_basket.
        counts[ count ] = counts[ count ] - 1.
        group_size = group_size + 1.
      ENDLOOP.

      " Calculate discount for the group and add to total price
      IF group_size > 0.
        price = price + calculate_discount( group_size ).
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
