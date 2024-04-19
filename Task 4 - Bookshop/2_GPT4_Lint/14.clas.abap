CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount_price) TYPE p LENGTH 3 DECIMALS 2.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          max_group   TYPE i VALUE 5,
          discount_price TYPE p LENGTH 3 DECIMALS 2.

    FIELD-SYMBOLS: <book_count> LIKE LINE OF book_counts.

    " Count the number of each book
    LOOP AT basket INTO DATA(book).
      ASSIGN book_counts[ book ] TO <book_count>.
      IF <book_count> IS ASSIGNED.
        <book_count> = <book_count> + 1.
      ELSE.
        book_counts[ book ] = 1.
      ENDIF.
    ENDLOOP.

    " Initialize total cost
    total = 0.

    " Calculate the total by trying to maximize the discount
    WHILE lines( book_counts ) > 0.
      DATA(num_books) = 0.
      LOOP AT book_counts INTO DATA(book_count) FROM max_group WHILE num_books < max_group.
        IF book_count > 0.
          num_books = num_books + 1.
          book_counts[ sy-tabix ] = book_count - 1.
        ENDIF.
      ENDLOOP.

      " Remove empty entries
      DELETE book_counts WHERE table_line = 0.

      " Calculate the discount for the group
      discount_price = calculate_discount( num_books ).
      total = total + num_books * discount_price.
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discount_price = 8.00.
      WHEN 2.
        discount_price = 8 * 0.95.
      WHEN 3.
        discount_price = 8 * 0.90.
      WHEN 4.
        discount_price = 8 * 0.80.
      WHEN 5.
        discount_price = 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
