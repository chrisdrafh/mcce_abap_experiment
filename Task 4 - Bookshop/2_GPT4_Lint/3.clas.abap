CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p LENGTH 6 DECIMALS 2.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          max_group_size TYPE i VALUE 5,
          num_different_books TYPE i,
          discounted_price TYPE p LENGTH 6 DECIMALS 2.

    " Count each book occurrence
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Sort the counts to ensure the most frequent books are considered last
    SORT book_counts DESCENDING.

    " Initialize total price
    total = 0.

    " Calculate the total price by applying the largest possible discounts first
    WHILE lines( book_counts ) > 0.
      " Determine how many different books are there (up to 5)
      num_different_books = lines( book_counts ) > max_group_size ? max_group_size : lines( book_counts ).

      " Get discounted price for this group size
      discounted_price = calculate_discount( num_different_books ).

      " Update total price
      total = total + ( discounted_price * num_different_books ).

      " Decrease count of each book and remove from list if zero
      DELETE book_counts INDEX 1 WHERE book_counts[ 1 ] < 2.
      MODIFY book_counts FROM 1 TRANSPORTING VALUE WHERE book_counts[ sy-tabix ] >= 1.
      IF sy-subrc = 0.
        book_counts[ sy-tabix ] = book_counts[ sy-tabix ] - 1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discounted_price = 8.0.
      WHEN 2.
        discounted_price = 8.0 * (1 - 0.05).
      WHEN 3.
        discounted_price = 8.0 * (1 - 0.10).
      WHEN 4.
        discounted_price = 8.0 * (1 - 0.20).
      WHEN 5.
        discounted_price = 8.0 * (1 - 0.25).
      WHEN OTHERS.
        discounted_price = 8.0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
