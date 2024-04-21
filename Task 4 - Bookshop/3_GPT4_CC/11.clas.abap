CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket          TYPE basket_type
      RETURNING VALUE(book_qty) TYPE TABLE OF i INDEX TABLE.

    METHODS calculate_discounted_price
      IMPORTING
        num_books   TYPE i
        book_count  TYPE i
      RETURNING VALUE(price) TYPE p LENGTH 5 DECIMALS 2.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_qty TYPE TABLE OF i INDEX TABLE,
          prices TYPE TABLE OF p LENGTH 5 DECIMALS 2,
          num_books TYPE i VALUE 5.

    book_qty = count_books( basket ).

    WHILE num_books > 1.
      DATA(current_books) = num_books.
      WHILE book_qty[current_books] >= num_books.
        APPEND calculate_discounted_price( num_books, 1 ) TO prices.
        book_qty[current_books] = book_qty[current_books] - num_books.
      ENDWHILE.
      num_books = num_books - 1.
    ENDWHILE.

    total = REDUCE #( INIT sum TYPE p LENGTH 5 DECIMALS 2 FOR price IN prices NEXT sum = sum + price ).
  ENDMETHOD.

  METHOD count_books.
    book_qty = VALUE #( ( 5 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ). " Initialize with 5 entries
    LOOP AT basket INTO DATA(book).
      book_qty[book] = book_qty[book] + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_discounted_price.
    DATA(book_price) = 8. " Price per book
    DATA(discount) = VALUE p( ).
    CASE num_books.
      WHEN 2.
        discount = book_price * num_books * 0.05.
      WHEN 3.
        discount = book_price * num_books * 0.10.
      WHEN 4.
        discount = book_price * num_books * 0.20.
      WHEN 5.
        discount = book_price * num_books * 0.25.
    ENDCASE.
    price = book_price * num_books - discount * book_count.
  ENDMETHOD.

ENDCLASS.
