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
    METHODS calculate_discount
      IMPORTING
        counts      TYPE STANDARD TABLE OF i
      RETURNING
        VALUE(cost) TYPE total.

    CONSTANTS: base_price TYPE p LENGTH 10 DECIMALS 2 VALUE 8.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE STANDARD TABLE OF i,
          unique_books TYPE i.

    " Count occurrences of each book
    LOOP AT basket INTO DATA(book).
      IF sy-tabix > counts[ book ].
        counts[ book ] = 1.
      ELSE.
        counts[ book ] = counts[ book ] + 1.
      ENDIF.
    ENDLOOP.

    " Call the recursive function to calculate minimum cost
    total = calculate_discount( counts ).
  ENDMETHOD.

  METHOD calculate_discount.
    DATA: min_cost TYPE p LENGTH 10 DECIMALS 2 VALUE '99999999.99',
          discount_table TYPE TABLE OF p LENGTH 10 DECIMALS 2,
          prices TYPE STANDARD TABLE OF p LENGTH 10 DECIMALS 2 WITH EMPTY KEY.

    " Discounts based on unique set sizes
    discount_table = VALUE #(
      ( COND #( WHEN lines( counts ) = 1 THEN base_price * 0.95 ELSE 0 ) )
      ( COND #( WHEN lines( counts ) = 2 THEN base_price * 0.90 ELSE 0 ) )
      ( COND #( WHEN lines( counts ) = 3 THEN base_price * 0.80 ELSE 0 ) )
      ( COND #( WHEN lines( counts ) = 4 THEN base_price * 0.75 ELSE 0 ) )
      ( COND #( WHEN lines( counts ) = 5 THEN base_price * 0.60 ELSE 0 ) )
    ).

    " Try different combinations by removing books
    LOOP AT counts INTO DATA(count) FROM 1 WHILE count > 0.
      DATA(new_counts) = counts.
      new_counts[ sy-tabix ] = new_counts[ sy-tabix ] - 1.
      DELETE new_counts WHERE table_line = 0.

      " Recursively calculate cost
      prices = VALUE #( BASE prices ( calculate_discount( new_counts ) + discount_table[ lines( new_counts ) ] ) ).
    ENDLOOP.

    IF prices IS NOT INITIAL.
      min_cost = REDUCE #( INIT min TYPE p LENGTH 10 DECIMALS 2 FOR price IN prices NEXT min = COND #( WHEN price < min THEN price ELSE min ) ).
    ENDIF.

    RETURN min_cost.
  ENDMETHOD.

ENDCLASS.
