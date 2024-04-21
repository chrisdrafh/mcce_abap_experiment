CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES: book_id TYPE i.

    TYPES: basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES: total TYPE p LENGTH 7 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: book_count_type TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    METHODS find_best_price
      IMPORTING
        books TYPE book_count_type
      RETURNING
        VALUE(price) TYPE total.

    CLASS-METHODS count_books
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(book_counts) TYPE book_count_type.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE book_count_type,
          price       TYPE total.

    book_counts = count_books( basket ).
    price = find_best_price( book_counts ).

    total = price.
  ENDMETHOD.

  METHOD find_best_price.
    DATA: discount TYPE total VALUE 8,
          counts TYPE i,
          discounts TYPE TABLE OF total WITH EMPTY KEY,
          temp_books TYPE book_count_type.

    " Initialize discounts
    discounts = VALUE #( ( 0 ) ( 0.95 * discount ) ( 0.90 * discount ) ( 0.80 * discount ) ( 0.75 * discount ) ).

    WHILE lines( books ) > 0.
      temp_books = books.

      counts = 5.
      WHILE counts > 0.
        IF count_books_in_set( temp_books, counts ) = counts.
          " Calculate price for this set
          price = price + counts * discounts[ counts ].
          " Remove books from set
          decrement_books( temp_books, counts ).
        ELSE.
          counts = counts - 1.
        ENDIF.
      ENDWHILE.
    ENDWHILE.
  ENDMETHOD.

  METHOD count_books.
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD decrement_books.
    DATA: idx TYPE i.
    idx = 5.
    WHILE idx > 0 AND counts > 0.
      IF books[ idx ] > 0.
        books[ idx ] = books[ idx ] - 1.
        counts = counts - 1.
      ENDIF.
      idx = idx - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD count_books_in_set.
    DATA: total TYPE i.
    LOOP AT books INTO DATA(book).
      IF book > 0.
        total = total + 1.
      ENDIF.
    ENDLOOP.
    RETURN total.
  ENDMETHOD.

ENDCLASS.
