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
    METHODS find_best_discount
      IMPORTING
        counts       TYPE STANDARD TABLE OF i
      RETURNING
        VALUE(cost)  TYPE total.

    METHODS calculate_discount
      IMPORTING
        num_books    TYPE i
      RETURNING
        VALUE(price) TYPE total.

ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INITIAL SIZE 5,
          max_books TYPE i,
          idx TYPE i.

    " Initialize the counts of each book to zero
    book_counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      book_counts[ book - 1 ] = book_counts[ book - 1 ] + 1.
    ENDLOOP.

    " Sort the counts in descending order to prioritize larger discounts for books with higher counts
    SORT book_counts DESCENDING.

    " Calculate the best possible discount
    total = find_best_discount( book_counts ).
  ENDMETHOD.

  METHOD find_best_discount.
    " Recursively find the best discount setup
    DATA: possible_costs TYPE STANDARD TABLE OF total WITH DEFAULT KEY.

    " Check if we still have books
    IF counts IS INITIAL.
      cost = 0.
    ELSE.
      " Check each possible group size from 5 to 1
      DO 5 TIMES.
        DATA(group_size) = sy-index.
        DATA(temp_counts) = counts.

        " If we can form a group of 'group_size'
        IF temp_counts[ group_size ] >= 1.
          " Reduce the count of each of the first 'group_size' books
          DO group_size TIMES.
            idx = sy-index.
            IF temp_counts[ idx - 1 ] > 0.
              temp_counts[ idx - 1 ] = temp_counts[ idx - 1 ] - 1.
            ENDIF.
          ENDDO.

          " Clean up zero entries
          DELETE temp_counts WHERE table_line = 0.

          " Calculate cost for this grouping and recursively for the rest
          APPEND calculate_discount( group_size ) + find_best_discount( temp_counts ) TO possible_costs.
        ENDIF.
      ENDDO.

      " Pick the lowest cost among all possible groupings
      SORT possible_costs.
      READ TABLE possible_costs INTO cost INDEX 1.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_discount.
    " Calculate price based on the discount for the number of different books
    CASE num_books.
      WHEN 1.
        price = 8.
      WHEN 2.
        price = 2 * 8 * ( 1 - 0.05 ).
      WHEN 3.
        price = 3 * 8 * ( 1 - 0.10 ).
      WHEN 4.
        price = 4 * 8 * ( 1 - 0.20 ).
      WHEN 5.
        price = 5 * 8 * ( 1 - 0.25 ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
