CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS find_best_price
      IMPORTING
        basket       TYPE basket_type
      RETURNING
        VALUE(price) TYPE total.

    METHODS count_books
      IMPORTING
        books        TYPE basket_type
      RETURNING
        VALUE(counts) TYPE STANDARD TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

    METHODS calculate_price_for_set
      IMPORTING
        num_books    TYPE i
      RETURNING
        VALUE(price) TYPE p LENGTH 10 DECIMALS 2.

    CONSTANTS: c_book_price TYPE p LENGTH 10 DECIMALS 2 VALUE '8.00'.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_best_price( basket ).
  ENDMETHOD.

  METHOD find_best_price.
    DATA: counts TYPE STANDARD TABLE OF i WITH NON-UNIQUE DEFAULT KEY.
    counts = count_books( basket ).
    price = 0.

    DO 5 TIMES.
      CASE sy-index.
        WHEN 5.
          IF counts[ 5 ] > 0.
            price = price + calculate_price_for_set( 5 ) * counts[ 5 ].
            counts = counts - 1.
          ENDIF.
        WHEN 4.
          IF counts[ 4 ] > 0.
            price = price + calculate_price_for_set( 4 ) * counts[ 4 ].
            counts = counts - 1.
          ENDIF.
        WHEN 3.
          IF counts[ 3 ] > 0.
            price = price + calculate_price_for_set( 3 ) * counts[ 3 ].
            counts = counts - 1.
          ENDIF.
        WHEN 2.
          IF counts[ 2 ] > 0.
            price = price + calculate_price_for_set( 2 ) * counts[ 2 ].
            counts = counts - 1.
          ENDIF.
        WHEN 1.
          IF counts[ 1 ] > 0.
            price = price + calculate_price_for_set( 1 ) * counts[ 1 ].
            counts = counts - 1.
          ENDIF.
      ENDCASE.
    ENDDO.

    price = price + SUM( counts ) * c_book_price.
  ENDMETHOD.

  METHOD count_books.
    DATA: book_count TYPE TABLE OF i WITH NON-UNIQUE DEFAULT KEY INITIAL SIZE 5.
    FIELD-SYMBOLS: <count> LIKE LINE OF book_count.

    book_count = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).

    LOOP AT books INTO DATA(book).
      ASSIGN COMPONENT book OF STRUCTURE book_count TO <count>.
      IF sy-subrc = 0.
        <count> = <count> + 1.
      ENDIF.
    ENDLOOP.

    counts = book_count.
  ENDMETHOD.

  METHOD calculate_price_for_set.
    CASE num_books.
      WHEN 1.
        price = c_book_price.
      WHEN 2.
        price = c_book_price * 2 * 0.95.
      WHEN 3.
        price = c_book_price * 3 * 0.90.
      WHEN 4.
        price = c_book_price * 4 * 0.80.
      WHEN 5.
        price = c_book_price * 5 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
