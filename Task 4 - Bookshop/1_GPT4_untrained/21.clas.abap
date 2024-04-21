CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF book_count,
             book_id TYPE i,
             count   TYPE i,
           END OF book_count.

    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.

    TYPES book_counts TYPE SORTED TABLE OF book_count
      WITH UNIQUE KEY book_id.

    CLASS-METHODS get_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE f.

    METHODS calculate_optimal_cost
      IMPORTING
        books TYPE book_counts
      RETURNING
        VALUE(optimal_cost) TYPE total.

    METHODS count_books
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(books) TYPE book_counts.

ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: books TYPE book_counts,
          optimal_cost TYPE total.

    books = count_books( basket ).
    optimal_cost = calculate_optimal_cost( books ).
    total = optimal_cost.
  ENDMETHOD.

  METHOD get_discount.
    CASE num_books.
      WHEN 2.
        discount = 0.95.
      WHEN 3.
        discount = 0.90.
      WHEN 4.
        discount = 0.80.
      WHEN 5.
        discount = 0.75.
      WHEN OTHERS.
        discount = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_optimal_cost.
    " Implementation of the optimal discount calculation goes here.
    " For demonstration purposes, assume that we only handle direct counts.
    DATA: price_per_book TYPE p LENGTH 8 DECIMALS 2 VALUE 8.
    optimal_cost = 0.

    LOOP AT books INTO DATA(book).
      optimal_cost = optimal_cost + book-count * price_per_book * get_discount( book-count ).
    ENDLOOP.

  ENDMETHOD.

  METHOD count_books.
    LOOP AT basket INTO DATA(bk).
      DATA(existing_entry) = VALUE #( BASE books[ table_line = bk ] OPTIONAL ).
      IF existing_entry IS INITIAL.
        books = VALUE #( BASE books ( book_id = bk count = 1 ) ).
      ELSE.
        books[ table_line = bk ]-count = books[ table_line = bk ]-count + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
