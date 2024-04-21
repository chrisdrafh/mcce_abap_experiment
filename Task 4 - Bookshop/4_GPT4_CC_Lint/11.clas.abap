CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_group_discount
      IMPORTING
        number_of_different_books TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p LENGTH 3 DECIMALS 2.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line,
          group_sizes TYPE STANDARD TABLE OF i,
          discounted_price TYPE p LENGTH 3 DECIMALS 2,
          max_discount_groups TYPE i,
          num_books TYPE i,
          book TYPE book_id.

    " Count each book occurrence
    LOOP AT basket INTO book.
      book_counts[ book ] = COND #( WHEN book_counts[ book ] IS NOT INITIAL THEN book_counts[ book ] + 1 ELSE 1 ).
    ENDLOOP.

    " Try to form groups of 5 down to 2 for maximum discount
    DO 5 TIMES.
      max_discount_groups = sy-index - 1.
      num_books = lines( book_counts ).
      WHILE num_books >= max_discount_groups AND max_discount_groups >= 2.
        " Determine possible number of groups with current max size
        group_sizes = VALUE #( FOR book IN book_counts WHERE ( book IS NOT INITIAL AND book >= max_discount_groups ) ( max_discount_groups ) ).
        IF lines( group_sizes ) >= max_discount_groups.
          " Apply discount for this group size
          discounted_price += calculate_group_discount( max_discount_groups ).
          " Reduce count for each book in this group
          LOOP AT group_sizes INTO num_books.
            LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<book_count>).
              IF <book_count> > 0.
                <book_count> = <book_count> - 1.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
        num_books = lines( book_counts ).
      ENDWHILE.
    ENDDO.

    " Calculate any remaining books at full price
    LOOP AT book_counts INTO num_books.
      total += num_books * 8.  " Full price, no discount
    ENDLOOP.

    " Add discounted prices
    total += discounted_price.
  ENDMETHOD.

  METHOD calculate_group_discount.
    CASE number_of_different_books.
      WHEN 5.
        discounted_price = 5 * 8 * 0.75. " 25% discount
      WHEN 4.
        discounted_price = 4 * 8 * 0.80. " 20% discount
      WHEN 3.
        discounted_price = 3 * 8 * 0.90. " 10% discount
      WHEN 2.
        discounted_price = 2 * 8 * 0.95. " 5% discount
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
