CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    TYPES: tt_count TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE f.
    METHODS get_max_discount_groups
      IMPORTING
        book_counts TYPE tt_count
      RETURNING
        VALUE(best_price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = VALUE tt_count( DEFAULT KEY COMPONENTS table_line = 0 FOR book_id = 1 TO 5 ).
    DATA(book) LIKE LINE OF book_counts.

    " Count each book in the basket
    LOOP AT basket INTO DATA(book_id).
      book = VALUE #( table_line = book_id COMPONENTS table_line = 1 ).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Calculate the total price with the best discount
    total = get_max_discount_groups( book_counts ).
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 2.
        discount = 0.95.
      WHEN 3.
        discount = 0.90.
      WHEN 4.
        discount = 0.80.
      WHEN 5.
        discount = 0.75.
      WHEN OTHERS.
        discount = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD get_max_discount_groups.
    DATA(possible_discounts) = VALUE tt_count( DEFAULT KEY COMPONENTS table_line = 0 FOR num_books = 5 TO 1 BY -1 ).
    DATA(price) TYPE total.
    DATA(max_discount) TYPE f.

    " Attempt to create groups for maximum discount
    DO 5 TIMES.
      DATA(group_size) = sy-index.
      max_discount = calculate_discount( group_size ).

      WHILE book_counts[ group_size ] >= group_size.
        " Apply discount for this group
        price += group_size * 8 * max_discount.
        " Reduce count of each book type by the size of this group
        book_counts = REDUCE tt_count( INIT x TYPE tt_count FOR j = 1 TO 5
                                        NEXT x[ j ] = x[ j ] - COND #( WHEN book_counts[ j ] >= group_size THEN group_size ELSE 0 ) ).
      ENDWHILE.
    ENDDO.

    best_price = price.
  ENDMETHOD.

ENDCLASS.
