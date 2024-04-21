CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! ID of book to buy from 1 to 5
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    "! Method to calculate total cost of the basket applying maximum discounts
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    "! Helper methods to handle discounts and calculate optimal price
    METHODS calculate_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_optimal_price
      IMPORTING
        books TYPE basket_type
      RETURNING
        VALUE(best_price) TYPE total.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: best_price TYPE total.
    best_price = calculate_optimal_price( basket ).
    total = best_price.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 2.
        discount = 0.95.
      WHEN 3.
        discount = 0.90.
      WHEN 4.
        discount = 0.80.
      WHEN 5.
        discount = 0.75.
      WHEN OTHERS.
        discount = 1.00.
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_optimal_price.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          price TYPE total,
          subset TYPE basket_type,
          i TYPE i,
          j TYPE i,
          temp_price TYPE total.

    " Count occurrences of each book
    LOOP AT books INTO DATA(book).
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

    " Base case: if all counts are zero
    IF lines( counts ) = 0.
      best_price = 0.
      RETURN.
    ENDIF.

    best_price = 999999. " Arbitrarily large number

    " Generate subsets for discounts
    DO 5 TIMES.
      CLEAR: subset, price.
      " Create potential subsets for discount application
      DO lines( counts ) TIMES.
        IF counts[ sy-index ] > 0.
          APPEND sy-index TO subset.
          counts[ sy-index ] = counts[ sy-index ] - 1.
        ENDIF.
      ENDDO.

      " Calculate price for this subset
      temp_price = calculate_optimal_price( counts ).
      price = xstrlen( subset ) * 8 * calculate_discount( xstrlen( subset ) ) + temp_price.

      " Compare to find best price
      IF price < best_price.
        best_price = price.
      ENDIF.

      " Restore counts
      LOOP AT subset INTO j.
        counts[ j ] = counts[ j ] + 1.
      ENDLOOP.

      " Exit if subset is empty (no more combinations)
      IF subset IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
