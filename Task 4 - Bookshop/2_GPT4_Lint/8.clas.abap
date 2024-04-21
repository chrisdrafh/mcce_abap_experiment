CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE STANDARD TABLE OF i INDEX TABLE,
          max_discount TYPE p LENGTH 5 DECIMALS 2.

    FIELD-SYMBOLS: <book_id> TYPE i.

    " Count the occurrences of each book ID
    LOOP AT basket ASSIGNING <book_id>.
      counts[ <book_id> ] = counts[ <book_id> ] + 1.
    ENDLOOP.

    " Function to calculate price based on groups
    total = get_optimal_price( counts ).

  ENDMETHOD.

  METHOD get_optimal_price.
    IMPORTING
      counts      TYPE STANDARD TABLE OF i INDEX TABLE
    RETURNING
      VALUE(price) TYPE total.

    TYPES: BEGIN OF book_group,
             count   TYPE i,
             discount TYPE p LENGTH 5 DECIMALS 2,
           END OF book_group.

    DATA: groups TYPE TABLE OF book_group,
          temp_counts LIKE counts.

    " Define discounts
    groups = VALUE #( ( count = 5 discount = 0.75 )
                      ( count = 4 discount = 0.80 )
                      ( count = 3 discount = 0.90 )
                      ( count = 2 discount = 0.95 )
                      ( count = 1 discount = 1.00 ) ).

    " Sort groups to try biggest discounts first
    SORT groups BY count DESCENDING.

    " Recursive or greedy strategy to find optimal price
    price = 0.
    DO.
      CLEAR temp_counts.
      temp_counts = counts.

      " Attempt to form each group and calculate price
      LOOP AT groups INTO DATA(group).
        WHILE group_possible( temp_counts, group-count ).
          price = price + group-count * 8 * group-discount.
          " Reduce the counts
          REDUCE i = 1 UNTIL i > group-count INTO DATA(j).
            temp_counts[ i ] = temp_counts[ i ] - 1.
          ENDREDUCE.
        ENDWHILE.
      ENDLOOP.

      " Check if all books are processed
      IF lines( temp_counts ) = 0.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD group_possible.
    IMPORTING
      counts      TYPE STANDARD TABLE OF i INDEX TABLE
      num_books   TYPE i
    RETURNING
      VALUE(possible) TYPE abap_bool.

    DATA(book_count) = REDUCE i( INIT x = 0 FOR <book> IN counts WHILE x < num_books NEXT x = x + COND #( WHEN <book> > 0 THEN 1 ELSE 0 ) ).
    possible = xsdbool( book_count >= num_books ).

  ENDMETHOD.

ENDCLASS.
