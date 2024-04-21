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

  PRIVATE SECTION.
    TYPES:
      tt_counts TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.

    METHODS count_books
      IMPORTING
        basket TYPE basket_type
      RETURNING VALUE(counts) TYPE tt_counts.

    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING VALUE(discount_price) TYPE p LENGTH 8 DECIMALS 2.

ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(counts) = count_books( basket ).
    DATA(max_group_size) TYPE i VALUE 5.
    DATA(price) TYPE p LENGTH 8 DECIMALS 2.
    total = 0.

    WHILE max_group_size > 0.
      WHILE counts[max_group_size] > 0.
        " Apply discount for max possible group size
        price = calculate_discount( max_group_size ).
        total = total + price * max_group_size.
        counts[max_group_size] = counts[max_group_size] - 1.

        " Update counts for each book type
        LOOP AT counts FROM 1 UNTIL max_group_size INTO DATA(book_count).
          IF book_count > 0 THEN
            counts[book_count] = counts[book_count] - 1.
          ENDIF.
        ENDLOOP.
      ENDWHILE.
      max_group_size = max_group_size - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD count_books.
    " Counts each unique book
    counts = VALUE #( ( FOR <book> IN basket ( <book> = 1 ) ) GROUP BY ( <book> ) ASCENDING ).

    " Fill in zero counts for easier handling
    DO 5 TIMES.
      IF counts[ sy-index ] IS INITIAL.
        counts[ sy-index ] = 0.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discount_price = 8.
      WHEN 2.
        discount_price = 8 * (1 - 0.05).
      WHEN 3.
        discount_price = 8 * (1 - 0.10).
      WHEN 4.
        discount_price = 8 * (1 - 0.20).
      WHEN 5.
        discount_price = 8 * (1 - 0.25).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
