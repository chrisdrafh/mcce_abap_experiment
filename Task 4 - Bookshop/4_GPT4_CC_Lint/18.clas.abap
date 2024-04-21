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
    METHODS compute_best_price
      IMPORTING
        count TYPE i
      RETURNING
        VALUE(price) TYPE p LENGTH 5 DECIMALS 2.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE SORTED TABLE OF i INDEX TABLE
                        WITH NON-UNIQUE KEY table_line,
          max_count   TYPE i,
          price       TYPE p LENGTH 5 DECIMALS 2,
          total_price TYPE p LENGTH 5 DECIMALS 2.

    " Count the occurrences of each book
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Calculate price until all books are processed
    WHILE lines( book_counts ) > 0.
      CLEAR max_count.
      LOOP AT book_counts INTO DATA(count).
        max_count = count.
      ENDLOOP.

      " Compute price for the maximum set of unique books
      price = compute_best_price( lines( book_counts ) ).

      " Reduce the count and adjust total price
      LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<count>).
        <count> = <count> - 1.
        IF <count> = 0.
          DELETE book_counts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      total_price += price * max_count.
    ENDWHILE.

    total = total_price.
  ENDMETHOD.

  METHOD compute_best_price.
    CASE count.
      WHEN 1.
        price = 8.00.
      WHEN 2.
        price = 2 * 8 * 0.95.
      WHEN 3.
        price = 3 * 8 * 0.90.
      WHEN 4.
        price = 4 * 8 * 0.80.
      WHEN 5.
        price = 5 * 8 * 0.75.
      WHEN OTHERS.
        price = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
