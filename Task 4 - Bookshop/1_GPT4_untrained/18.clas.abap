CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS group_books
      IMPORTING
        iv_array TYPE basket_type
      RETURNING
        VALUE(rv_grouped_books) TYPE basket_type.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          group_sizes TYPE basket_type,
          group_index TYPE i,
          discounts TYPE STANDARD TABLE OF decfloat16,
          book_price TYPE decfloat16 VALUE 8,
          max_group_size TYPE i VALUE 5.

    " Initialize discounts
    discounts = VALUE #( ( 0.0 ) ( 0.05 ) ( 0.1 ) ( 0.2 ) ( 0.25 ) ).

    " Count each book occurrences
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Group books to maximize discounts
    WHILE lines( book_counts ) > 0.
      group_sizes = group_books( VALUE #( FOR book IN book_counts WHERE ( book > 0 ) ( book ) ) ).
      LOOP AT group_sizes INTO group_index.
        " Calculate discount for each group and decrement count
        total = total + ( max_group_size - group_index + 1 ) * book_price * ( 1 - discounts[ group_index ] ).
        book_counts[ group_index ] = book_counts[ group_index ] - 1.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.

  METHOD group_books.
    " Determine the best grouping based on remaining book counts
    DATA(lv_max_books) = REDUCE i( INIT lv_count TYPE i FOR book IN iv_array NEXT lv_count = lv_count + 1 ).

    " Handle different scenarios based on the number of different books
    CASE lv_max_books.
      WHEN 1.
        rv_grouped_books = VALUE #( ( 1 ) ).
      WHEN 2.
        rv_grouped_books = VALUE #( ( 2 ) ).
      WHEN 3.
        rv_grouped_books = VALUE #( ( 3 ) ).
      WHEN 4.
        rv_grouped_books = VALUE #( ( 4 ) ).
      WHEN 5.
        rv_grouped_books = VALUE #( ( 5 ) ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
