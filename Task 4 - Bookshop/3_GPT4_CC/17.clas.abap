CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2,
           t_count TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(counts) TYPE t_count.

    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING VALUE(price) TYPE p.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE t_count,
          num_groups TYPE i,
          group_size TYPE i,
          index TYPE i,
          subtotal TYPE p.

    counts = count_books( basket ).

    DO 5 TIMES.
      group_size = 6 - sy-index.

      " Find how many full groups of this size can be formed
      num_groups = counts[ sy-index ].
      LOOP AT counts FROM sy-index INTO index.
        num_groups = LEAST( num_groups, counts[ index ] ).
      ENDLOOP.

      IF num_groups > 0 THEN
        subtotal = num_groups * calculate_discount( group_size ).
        total = total + subtotal.

        " Decrease the counts for each book used in the groups
        LOOP AT counts FROM sy-index INTO index.
          counts[ index ] = counts[ index ] - num_groups.
        ENDLOOP.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD count_books.
    DATA(book TYPE book_id).

    counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT basket INTO book.
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        price = 8.
      WHEN 2.
        price = 2 * 8 * 0.95.
      WHEN 3.
        price = 3 * 8 * 0.90.
      WHEN 4.
        price = 4 * 8 * 0.80.
      WHEN 5.
        price = 5 * 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
