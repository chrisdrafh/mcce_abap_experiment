CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      book_count TYPE i,
      book_table TYPE SORTED TABLE OF book_count WITH UNIQUE KEY table_line.

    METHODS calculate_total
      IMPORTING basket TYPE book_table
      RETURNING VALUE(total) TYPE p LENGTH 8 DECIMALS 2.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p LENGTH 8 DECIMALS 2.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: max_books TYPE i VALUE 5,
          possible_sets TYPE book_table,
          book_set TYPE book_count.

    FIELD-SYMBOLS: <fs_book> TYPE book_count.

    " Initialize the possible sets count
    DO max_books TIMES.
      book_set = sy-index.
      INSERT book_set INTO TABLE possible_sets.
    ENDDO.

    " Initialize variables for dynamic programming
    DATA(book_prices) = VALUE book_table( FOR i = 1 THEN i + 1 UNTIL i > max_books
                                          ( book_count = calculate_discount( i ) * i ) ).

    " Count occurrences of each book
    DATA(book_counts) = VALUE book_table( FOR <fs_book> IN basket
                                          ( book_count = lines( VALUE #( FOR j = 1 UNTIL j > <fs_book> 
                                                                        ( j ) ) ) ) ).

    " Apply dynamic programming to find the best discount arrangement
    " Further detailed logic needed for optimizing the book set combinations

    total = 0.
    DO.
      DATA(max_set_size) = REDUCE i( INIT max_size TYPE i FOR <fs_book> IN book_counts
                                      WHERE ( table_line = max_size )
                                      NEXT max_size = COND #( WHEN <fs_book> > max_size THEN <fs_book> ELSE max_size ) ).

      IF max_set_size = 0.
        EXIT.
      ENDIF.

      total += book_prices[ max_set_size ].
      " Decrease the count of books in the set
      book_counts = VALUE #( FOR <fs_book> IN book_counts THEN <fs_book> + 1 UNTIL <fs_book> IS INITIAL
                               ( table_line = <fs_book> - 1 ) ).
    ENDDO.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discounted_price = 8.
      WHEN 2.
        discounted_price = 8 * 0.95.
      WHEN 3.
        discounted_price = 8 * 0.9.
      WHEN 4.
        discounted_price = 8 * 0.8.
      WHEN 5.
        discounted_price = 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
