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
    METHODS find_optimal_set
      IMPORTING
        counts    TYPE TABLE OF i
        INDEX     TYPE i
        max_index TYPE i
      CHANGING
        solution TYPE TABLE OF i
        current  TYPE TABLE OF i.

    METHODS apply_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p LENGTH 3 DECIMALS 2.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INITIAL SIZE 5,
          solution TYPE TABLE OF i INITIAL SIZE 5,
          current TYPE TABLE OF i INITIAL SIZE 5.

    " Count the number of each book
    LOOP AT basket INTO DATA(book).
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

    " Find optimal set
    CLEAR solution.
    find_optimal_set( EXPORTING counts = counts
                                  INDEX = 1
                                  max_index = 5
                      CHANGING solution = solution
                               current = current ).

    " Calculate total price with maximum discount
    DATA: total_price TYPE p LENGTH 3 DECIMALS 2.
    LOOP AT solution INTO DATA(num_books).
      total_price = total_price + apply_discount( num_books ).
    ENDLOOP.

    total = total_price.
  ENDMETHOD.

  METHOD find_optimal_set.
    " Implementation needed for finding optimal set of books for discount
  ENDMETHOD.

  METHOD apply_discount.
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
