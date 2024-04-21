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

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_best_discount
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(result) TYPE total.
    METHODS apply_discount
      IMPORTING num_books    TYPE i
      RETURNING VALUE(price) TYPE total.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_best_discount( basket ).
  ENDMETHOD.

  METHOD find_best_discount.
    DATA: num_books TYPE i,
          book_count TYPE TABLE OF i INITIAL SIZE 5,
          temp_basket TYPE basket_type,
          max_discount TYPE total VALUE 0,
          current_discount TYPE total.

    " Count the books
    LOOP AT basket INTO DATA(book).
      book_count[ book ] = book_count[ book ] + 1.
    ENDLOOP.

    " Try to find best discount by checking the largest set first
    DO 5 TIMES.
      CLEAR: num_books, temp_basket.
      num_books = sy-index.

      " Check if a set of num_books can be made
      LOOP AT book_count INTO DATA(count) WHERE table_line > 0.
        IF count >= num_books.
          APPEND VALUE #( table_line = sy-tabix ) TO temp_basket TIMES num_books.
          book_count[ sy-tabix ] = book_count[ sy-tabix ] - num_books.
        ENDIF.
      ENDLOOP.

      " Calculate discount for the current set
      IF lines( temp_basket ) = num_books.
        current_discount = apply_discount( num_books ) * num_books.
        max_discount = max_discount + current_discount.
      ENDIF.

    ENDDO.

    " Calculate remaining books without discount
    LOOP AT book_count INTO DATA(remaining_count) WHERE table_line > 0.
      max_discount = max_discount + remaining_count * 8.00. " no discount price
    ENDLOOP.

    result = max_discount.
  ENDMETHOD.

  METHOD apply_discount.
    CASE num_books.
      WHEN 1.
        price = 8.00.
      WHEN 2.
        price = 8.00 * 0.95.
      WHEN 3.
        price = 8.00 * 0.90.
      WHEN 4.
        price = 8.00 * 0.80.
      WHEN 5.
        price = 8.00 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
