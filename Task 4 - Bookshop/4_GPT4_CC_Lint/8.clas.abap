CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_best_discount
      IMPORTING
        quantities TYPE i OCCURS 5
      RETURNING VALUE(price) TYPE total.

    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING VALUE(discount_price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: quantities TYPE i OCCURS 5 VALUE INITIAL.
    FIELD-SYMBOLS: <qty> LIKE LINE OF quantities.

    " Count quantities of each book
    LOOP AT basket INTO DATA(book).
      ASSIGN quantities[book - 1] TO <qty>.
      IF <qty> IS ASSIGNED.
        <qty> = <qty> + 1.
      ENDIF.
    ENDLOOP.

    " Calculate price based on best possible discount arrangement
    total = find_best_discount( quantities ).
  ENDMETHOD.

  METHOD find_best_discount.
    " Recursively find the best discount configuration
    DATA: remaining_books TYPE i,
          temp_price TYPE total,
          min_price TYPE total VALUE '999999.99'.

    DO 5 TIMES.
      IF quantities[ sy-index - 1 ] > 0.
        quantities[ sy-index - 1 ] = quantities[ sy-index - 1 ] - 1.
        remaining_books = sy-index.

        " Recursive call
        temp_price = calculate_discount( remaining_books ) + find_best_discount( quantities ).

        " Check for minimum price found
        IF temp_price < min_price.
          min_price = temp_price.
        ENDIF.

        " Add back the book to try next configuration
        quantities[ sy-index - 1 ] = quantities[ sy-index - 1 ] + 1.
      ENDIF.
    ENDDO.

    IF min_price = '999999.99'.
      price = 0.
    ELSE.
      price = min_price.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discount_price = 8.00.
      WHEN 2.
        discount_price = 2 * 8 * 0.95.
      WHEN 3.
        discount_price = 3 * 8 * 0.90.
      WHEN 4.
        discount_price = 4 * 8 * 0.80.
      WHEN 5.
        discount_price = 5 * 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
