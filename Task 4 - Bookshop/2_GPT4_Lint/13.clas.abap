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

  PRIVATE SECTION.
    TYPES: book_count_type TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    METHODS count_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(counts) TYPE book_count_type.

    METHODS calculate_discount_group
      IMPORTING num_different_books TYPE i
      RETURNING VALUE(discounted_price) TYPE p LENGTH 5 DECIMALS 2.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE book_count_type,
          total_price TYPE p LENGTH 5 DECIMALS 2 VALUE 0.

    book_counts = count_books( basket ).

    WHILE lines( book_counts ) > 0.
      DATA(max_set_size) = lines( book_counts ).
      DATA(current_discount_price) = calculate_discount_group( max_set_size ).

      LOOP AT book_counts INTO DATA(book_count) FROM max_set_size DOWN TO 1.
        IF book_count >= 1.
          book_count = book_count - 1.
          MODIFY book_counts FROM book_count TRANSPORTING table_line WHERE table_line = sy-tabix.
          IF book_count = 0.
            DELETE book_counts INDEX sy-tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.

      total_price = total_price + current_discount_price.
    ENDWHILE.

    total = total_price.
  ENDMETHOD.


  METHOD count_books.
    DATA: counts TYPE book_count_type.
    LOOP AT basket INTO DATA(book).
      DATA(count) = COND #( WHEN line_exists( counts[ table_line = book ] ) THEN counts[ table_line = book ] + 1 ELSE 1 ).
      counts[ table_line = book ] = count.
    ENDLOOP.
    counts = VALUE #( FOR group IN counts WHERE group IS NOT INITIAL ( group ) ).
    RETURN counts.
  ENDMETHOD.


  METHOD calculate_discount_group.
    CONSTANTS: base_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8.
    DATA: discount TYPE p LENGTH 5 DECIMALS 2.

    CASE num_different_books.
      WHEN 1.
        discount = base_price.
      WHEN 2.
        discount = base_price * 2 * 0.95.
      WHEN 3.
        discount = base_price * 3 * 0.90.
      WHEN 4.
        discount = base_price * 4 * 0.80.
      WHEN 5.
        discount = base_price * 5 * 0.75.
      WHEN OTHERS.
        discount = base_price * num_different_books.
    ENDCASE.

    RETURN discount.
  ENDMETHOD.

ENDCLASS.
