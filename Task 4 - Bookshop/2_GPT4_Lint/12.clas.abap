CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 7 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: price_per_book TYPE p LENGTH 7 DECIMALS 2 VALUE '8.00'.

    TYPES: tt_count TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS get_discount
      IMPORTING
        count TYPE i
      RETURNING
        VALUE(discount) TYPE p LENGTH 7 DECIMALS 2.

    METHODS maximize_discount
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(total_price) TYPE total.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = maximize_discount( basket ).
  ENDMETHOD.

  METHOD get_discount.
    CASE count.
      WHEN 2.
        discount = 0.05.
      WHEN 3.
        discount = 0.10.
      WHEN 4.
        discount = 0.20.
      WHEN 5.
        discount = 0.25.
      WHEN OTHERS.
        discount = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD maximize_discount.
    DATA(book_counts) = VALUE tt_count( ALLOCATION INITIAL SIZE 5 ).

    " Count the occurrences of each book ID
    LOOP AT basket INTO DATA(book_id).
      book_counts[ book_id ] += 1.
    ENDLOOP.

    " Process the maximum discount possible by trying to form the largest sets
    total_price = 0.
    WHILE lines( book_counts ) > 0.
      DATA(num_books) = lines( book_counts ).
      IF num_books > 5.
        num_books = 5.
      ENDIF.

      " Apply the discount
      DATA(current_discount) = get_discount( num_books ).
      total_price += num_books * price_per_book * ( 1 - current_discount ).

      " Reduce the count of each book type by 1
      DATA(new_counts) = VALUE tt_count( ).
      LOOP AT book_counts INTO DATA(count) WHERE count > 0.
        APPEND count - 1 TO new_counts.
      ENDLOOP.
      book_counts = CORRESPONDING #( new_counts EXCEPT TABLE_LINE ).
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
