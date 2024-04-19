CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    TYPES: books_count_type TYPE STANDARD TABLE OF i WITH DEFAULT KEY INITIAL SIZE 5.
    METHODS calculate_group_discount
      IMPORTING
        books_count TYPE books_count_type
        group_size  TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p.
    METHODS count_books
      IMPORTING
        basket         TYPE basket_type
      RETURNING
        VALUE(books_count) TYPE books_count_type.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: books_count TYPE books_count_type,
          possible_sizes TYPE TABLE OF i,
          max_group_discount TYPE p,
          group_discount TYPE p.

    possible_sizes = VALUE #( ( 5 ) ( 4 ) ( 3 ) ( 2 ) ).
    books_count = count_books( basket ).
    total = 0.

    LOOP AT possible_sizes INTO DATA(group_size).
      WHILE calculate_group_discount( books_count, group_size ) > 0 INTO group_discount.
        total = total + group_discount.
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_group_discount.
    DATA: num_books TYPE i,
          price_per_book TYPE p VALUE 8,
          discount TYPE p.

    discounted_price = 0.
    num_books = REDUCE i( INIT x TYPE i FOR j = 1 UNTIL j > group_size
                           NEXT x = x + COND #( WHEN books_count[j] > 0 THEN 1 ELSE 0 ) ).

    IF num_books = group_size.
      CASE group_size.
        WHEN 2.
          discount = 0.05.
        WHEN 3.
          discount = 0.10.
        WHEN 4.
          discount = 0.20.
        WHEN 5.
          discount = 0.25.
      ENDCASE.

      discounted_price = group_size * price_per_book * ( 1 - discount ).

      LOOP AT books_count REFERENCE INTO DATA(book) FROM 1 TO group_size.
        IF book->* > 0.
          book->* = book->* - 1.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD count_books.
    books_count = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT basket INTO DATA(book_id).
      books_count[ book_id ] = books_count[ book_id ] + 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
