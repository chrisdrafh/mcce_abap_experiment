CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 7 DECIMALS 2.

    METHODS: calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    TYPES: t_count TYPE i,
           t_book_count TYPE STANDARD TABLE OF t_count WITH DEFAULT KEY.

    METHODS: count_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(book_counts) TYPE t_book_count,
      get_discount
      IMPORTING num_books    TYPE i
      RETURNING VALUE(discount) TYPE decfloat16.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE t_book_count,
          price_per_book TYPE p LENGTH 7 DECIMALS 2 VALUE '8.00',
          discounts TYPE TABLE OF decfloat16,
          temp_total TYPE p LENGTH 7 DECIMALS 2,
          max_group_size TYPE i VALUE 5.

    book_counts = count_books( basket ).

    WHILE max_group_size > 1.
      IF book_counts[ max_group_size ] > 0.
        discounts = VALUE #( ( get_discount( max_group_size ) ) FOR i = 1 UNTIL i > book_counts[ max_group_size ] ).
        LOOP AT discounts INTO DATA(discount).
          temp_total += max_group_size * price_per_book * discount.
        ENDLOOP.
        max_group_size -= 1.
      ELSE.
        max_group_size -= 1.
      ENDIF.
    ENDWHILE.

    total = temp_total.

  ENDMETHOD.

  METHOD count_books.
    DATA(book_count) = VALUE t_book_count( DEFAULT KEY COMPONENTS 0 0 0 0 0 ).

    LOOP AT basket INTO DATA(book).
      book_count[ book ] += 1.
    ENDLOOP.

    book_counts = book_count.
  ENDMETHOD.

  METHOD get_discount.
    CASE num_books.
      WHEN 5.
        discount = 0.75. " 25% discount
      WHEN 4.
        discount = 0.80. " 20% discount
      WHEN 3.
        discount = 0.90. " 10% discount
      WHEN 2.
        discount = 0.95. " 5% discount
      WHEN OTHERS.
        discount = 1.00. " No discount
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
