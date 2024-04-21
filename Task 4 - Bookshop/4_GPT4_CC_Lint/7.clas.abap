CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF book_count,
             id    TYPE i,
             count TYPE i,
           END OF book_count,
           book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING
        books           TYPE basket_type
      RETURNING
        VALUE(discount) TYPE total.

    METHODS max_discount
      IMPORTING
        books           TYPE basket_type
      RETURNING
        VALUE(price)    TYPE total.

    CLASS-DATA: book_prices TYPE STANDARD TABLE OF total WITH DEFAULT KEY INITIAL SIZE 5.
    CLASS-METHODS: class_constructor.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD class_constructor.
    " Set up book prices based on discounts
    book_prices = VALUE #( ( 8 ) ( 8 * 0.95 ) ( 8 * 0.90 ) ( 8 * 0.80 ) ( 8 * 0.75 ) ).
  ENDMETHOD.

  METHOD calculate_total.
    total = max_discount( basket ).
  ENDMETHOD.

  METHOD calculate_discount.
    " Calculate discount for a set of books
    CASE lines( books ).
      WHEN 0.
        discount = 0.
      WHEN 1.
        discount = books[ 1 ] * book_prices[ 1 ].
      WHEN 2.
        discount = lines( books ) * book_prices[ 2 ].
      WHEN 3.
        discount = lines( books ) * book_prices[ 3 ].
      WHEN 4.
        discount = lines( books ) * book_prices[ 4 ].
      WHEN 5.
        discount = lines( books ) * book_prices[ 5 ].
      WHEN OTHERS.
        discount = lines( books ) * book_prices[ 1 ].
    ENDCASE.
  ENDMETHOD.

  METHOD max_discount.
    " Initialize variables
    DATA(max_price) = TYPE total.
    DATA(price) = TYPE total.
    DATA(book_combinations) = VALUE basket_type( ).

    " Get unique books from the basket
    LOOP AT books INTO DATA(book).
      APPEND VALUE #( table_line = book ) TO book_combinations.
    ENDLOOP.
    SORT book_combinations BY table_line.
    DELETE ADJACENT DUPLICATES FROM book_combinations.

    " Try all combinations of books for discounts
    DO lines( book_combinations ) TIMES.
      IF lines( book_combinations ) >= sy-index.
        DATA(current_books) = book_combinations( FROM 1 TO sy-index ).
        price = calculate_discount( current_books ).
        " Calculate the remaining books' price recursively
        price += max_discount( books - current_books ).
        IF max_price IS INITIAL OR price < max_price.
          max_price = price.
        ENDIF.
      ENDIF.
    ENDDO.

    price = max_price.
  ENDMETHOD.

ENDCLASS.
