CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount_rate) TYPE p.

    METHODS count_unique_books
      IMPORTING
        iv_basket TYPE basket_type
      RETURNING
        VALUE(book_counts) TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_unique_books( basket ).
    DATA(max_books) = lines( basket ).
    total = 0.

    WHILE max_books > 0.
      DATA(set_size) = 0.
      DATA(book_index) = 1.

      WHILE book_index <= 5 AND set_size < max_books.
        IF book_counts[ book_index ] > 0.
          set_size = set_size + 1.
          book_counts[ book_index ] = book_counts[ book_index ] - 1.
        ENDIF.
        book_index = book_index + 1.
      ENDWHILE.

      DATA(discount_rate) = get_discount( set_size ).
      total = total + set_size * ( 8 - 8 * discount_rate ).
      max_books = max_books - set_size.
    ENDWHILE.

  ENDMETHOD.

  METHOD get_discount.
    CASE num_books.
      WHEN 2.
        discount_rate = 0.05.
      WHEN 3.
        discount_rate = 0.10.
      WHEN 4.
        discount_rate = 0.20.
      WHEN 5.
        discount_rate = 0.25.
      WHEN OTHERS.
        discount_rate = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD count_unique_books.
    book_counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT iv_basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
