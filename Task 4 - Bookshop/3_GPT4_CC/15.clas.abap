CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING 
        num_books TYPE i
      RETURNING VALUE(discount) TYPE p.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          group_size TYPE i,
          price_per_book TYPE p VALUE 8,
          num_discount_books TYPE i,
          discounted_price TYPE p.

    " Initialize book counts
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Calculate total price with optimal grouping for discounts
    total = 0.
    DO 5 TIMES.
      group_size = 5 - sy-index + 1.
      WHILE group_size > 1.
        num_discount_books = group_size.
        LOOP AT book_counts INTO DATA(count) WHERE count >= 1.
          book_counts[ sy-tabix ] = count - 1.
          num_discount_books = num_discount_books - 1.
          IF num_discount_books = 0.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF num_discount_books = 0.
          discounted_price = group_size * price_per_book * ( 1 - calculate_discount( group_size ) ).
          total = total + discounted_price.
        ELSE.
          " Reset deducted book counts if full group not formed
          LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<book_count>) FROM sy-tabix - group_size + 1.
            <book_count> = <book_count> + 1.
          ENDLOOP.
        ENDIF.
      ENDWHILE.
    ENDDO.

    " Any remaining books are charged at full price
    LOOP AT book_counts INTO count.
      total = total + count * price_per_book.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 5.
        discount = 0.25.
      WHEN 4.
        discount = 0.20.
      WHEN 3.
        discount = 0.10.
      WHEN 2.
        discount = 0.05.
      WHEN OTHERS.
        discount = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
