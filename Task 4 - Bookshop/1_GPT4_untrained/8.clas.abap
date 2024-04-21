CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_best_discount
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(best_price) TYPE total.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_best_discount( basket ).
  ENDMETHOD.

  METHOD find_best_discount.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          unique_counts TYPE i,
          price TYPE p LENGTH 8 DECIMALS 2.

    FIELD-SYMBOLS: <count> LIKE LINE OF counts.

    " Count how many of each book we have
    LOOP AT basket INTO DATA(book).
      ASSIGN counts[ book ] TO <count>.
      IF <count> IS ASSIGNED.
        <count> = <count> + 1.
      ELSE.
        INSERT 1 INTO counts INDEX book.
      ENDIF.
    ENDLOOP.

    " Calculate discount price based on unique sets
    CLEAR best_price.
    DO.
      " Reset unique book count
      unique_counts = 0.
      price = 0.

      " Check each count to form a set
      LOOP AT counts INTO <count>.
        IF <count> > 0.
          unique_counts = unique_counts + 1.
          <count> = <count> - 1.
        ENDIF.
      ENDLOOP.

      " Determine discount based on the number of unique books
      CASE unique_counts.
        WHEN 1.
          price = price + 8.
        WHEN 2.
          price = price + 2 * 8 * 0.95.
        WHEN 3.
          price = price + 3 * 8 * 0.90.
        WHEN 4.
          price = price + 4 * 8 * 0.80.
        WHEN 5.
          price = price + 5 * 8 * 0.75.
      ENDCASE.

      " Accumulate best price
      best_price = best_price + price.

      " Exit loop if no more sets can be formed
      IF unique_counts = 0.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
