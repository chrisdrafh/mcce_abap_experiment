CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS count_books
      IMPORTING
        basket       TYPE basket_type
      RETURNING
        VALUE(counts) TYPE TABLE OF i INDEX TABLE.

    METHODS calculate_discount
      IMPORTING
        num_different TYPE i
      RETURNING
        VALUE(discount) TYPE f.

ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          price_per_book TYPE f VALUE 8,
          max_discount TYPE i,
          current_discount TYPE f,
          total_price TYPE f VALUE 0.

    counts = count_books( basket ).

    " Calculate maximum set size
    max_discount = lines( counts ) WHILE counts[ sy-index ] >= counts[ lines( counts ) ].

    WHILE max_discount > 0.
      current_discount = calculate_discount( max_discount ).
      total_price += max_discount * price_per_book * ( 1 - current_discount ).
      " Reduce the counts of each book type
      counts = VALUE #( FOR j = 1 THEN j + 1 UNTIL j > max_discount
                        ( counts[ j ] - 1 ) WHERE counts[ j ] > 0 ).

      max_discount = lines( counts ) WHILE counts[ sy-index ] >= counts[ lines( counts ) ].
    ENDWHILE.

    total = total_price.

  ENDMETHOD.

  METHOD count_books.
    DATA(book_count) = VALUE TABLE OF i( DEFAULT 0 OCCURS 5 ).

    " Count each book type
    LOOP AT basket INTO DATA(book).
      book_count[ book ] += 1.
    ENDLOOP.

    " Filter out zero counts and sort descending
    counts = VALUE #( FOR book IN book_count
                      WHERE book > 0
                      ( book ) SORT BY book DESCENDING ).

  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_different.
      WHEN 1.
        discount = 0.0.
      WHEN 2.
        discount = 0.05.
      WHEN 3.
        discount = 0.10.
      WHEN 4.
        discount = 0.20.
      WHEN 5.
        discount = 0.25.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
