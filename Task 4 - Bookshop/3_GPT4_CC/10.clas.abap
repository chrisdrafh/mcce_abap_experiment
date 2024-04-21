CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket               TYPE basket_type
      RETURNING VALUE(book_counts)   TYPE TABLE OF i INDEX TABLE.

    METHODS calculate_discount
      IMPORTING num_books            TYPE i
      RETURNING VALUE(discount_rate) TYPE p.

    METHODS find_optimal_distribution
      IMPORTING book_counts          TYPE TABLE OF i INDEX TABLE
      RETURNING VALUE(best_price)    TYPE p.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_books( basket ).
    total = find_optimal_distribution( book_counts ).
  ENDMETHOD.

  METHOD count_books.
    book_counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT basket INTO DATA(book_id).
      book_counts[ book_id ] = book_counts[ book_id ] + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 2.
        discount_rate = 0.05.
      WHEN 3.
        discount_rate = 0.10.
      WHEN 4.
        discount_rate = 0.20.
      WHEN 5.
        discount_rate = 0.25.
      WHEN OTHERS.
        discount_rate = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD find_optimal_distribution.
    DATA(price) = 0.
    DATA(remaining_books) = book_counts.

    WHILE sum( val = remaining_books ) > 0.
      DATA(group_size) = 0.
      LOOP AT remaining_books ASSIGNING FIELD-SYMBOL(<count>) FROM 1 TO 5 WHERE <count> > 0.
        <count> = <count> - 1.
        group_size = group_size + 1.
      ENDLOOP.

      price = price + group_size * 8 * ( 1 - calculate_discount( group_size ) ).
    ENDWHILE.

    best_price = price.
  ENDMETHOD.

ENDCLASS.
