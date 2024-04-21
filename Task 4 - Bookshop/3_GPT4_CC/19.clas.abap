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
    METHODS group_books
      IMPORTING
        ip_basket TYPE basket_type
      RETURNING VALUE(rp_price) TYPE total.

    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING VALUE(discounted_price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = group_books( basket ).
  ENDMETHOD.

  METHOD group_books.
    DATA: book_counts TYPE SORTED TABLE OF i INDEX TABLE,
          temp_basket TYPE basket_type,
          max_group_size TYPE i VALUE 5,
          group_size TYPE i,
          price TYPE total.

    " Count the occurrences of each book
    LOOP AT ip_basket INTO DATA(book).
      book_counts[ book ] = COND #( WHEN book_counts[ book ] IS INITIAL THEN 1 ELSE book_counts[ book ] + 1 ).
    ENDLOOP.

    " Try to form groups of 5 down to 2 books to maximize discounts
    DO max_group_size TIMES.
      group_size = max_group_size + 1 - sy-index.

      WHILE line_exists( book_counts[ TABLE_LINE ] WHERE table_line >= group_size ).
        " Decrease counts and calculate price
        LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<count>) WHERE table_line >= group_size.
          <count> = <count> - group_size.
          price = price + calculate_discount( group_size ).
          IF <count> = 0.
            DELETE book_counts INDEX sy-tabix.
          ENDIF.
        ENDLOOP.
      ENDWHILE.
    ENDDO.

    " Any remaining books are sold at full price ($8 each)
    LOOP AT book_counts INTO DATA(count).
      price = price + count * 8.
    ENDLOOP.

    rp_price = price.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discounted_price = 8.
      WHEN 2.
        discounted_price = 2 * 8 * 0.95.
      WHEN 3.
        discounted_price = 3 * 8 * 0.90.
      WHEN 4.
        discounted_price = 4 * 8 * 0.80.
      WHEN 5.
        discounted_price = 5 * 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
