CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    CONSTANTS: book_price TYPE p LENGTH 8 DECIMALS 2 VALUE '8.00',
               discount1 TYPE p LENGTH 3 DECIMALS 2 VALUE '0.95', " 5% discount
               discount2 TYPE p LENGTH 3 DECIMALS 2 VALUE '0.90', " 10% discount
               discount3 TYPE p LENGTH 3 DECIMALS 2 VALUE '0.80', " 20% discount
               discount4 TYPE p LENGTH 3 DECIMALS 2 VALUE '0.75'. " 25% discount

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE INITIAL SIZE 5,
          idx TYPE i,
          possible_groups TYPE i,
          discounted_price TYPE p LENGTH 8 DECIMALS 2.

    " Initialize counts
    DO 5 TIMES.
      APPEND 0 TO counts.
    ENDDO.

    " Count each book
    LOOP AT basket INTO idx.
      counts[idx] = counts[idx] + 1.
    ENDLOOP.

    " Sort descending to prioritize larger discounts
    SORT counts DESCENDING.

    " Calculate the total price with optimal discount grouping
    WHILE counts[1] > 0.
      CASE counts[1].
        WHEN 5.
          IF counts[5] > 0.
            discounted_price = 5 * book_price * discount4.
            total = total + discounted_price.
            counts = counts - 5.
          ENDIF.
        WHEN 4.
          possible_groups = REDUCE #(
            INIT j = 0
            FOR i = 1 UNTIL i > 4
            DO j = j + COND #( WHEN counts[i] > 0 THEN 1 ELSE 0 )
          ).
          IF possible_groups = 4.
            discounted_price = 4 * book_price * discount3.
            total = total + discounted_price.
            counts = counts - 4.
          ENDIF.
        WHEN 3.
          possible_groups = REDUCE #(
            INIT j = 0
            FOR i = 1 UNTIL i > 3
            DO j = j + COND #( WHEN counts[i] > 0 THEN 1 ELSE 0 )
          ).
          IF possible_groups = 3.
            discounted_price = 3 * book_price * discount2.
            total = total + discounted_price.
            counts = counts - 3.
          ENDIF.
        WHEN 2.
          possible_groups = REDUCE #(
            INIT j = 0
            FOR i = 1 UNTIL i > 2
            DO j = j + COND #( WHEN counts[i] > 0 THEN 1 ELSE 0 )
          ).
          IF possible_groups = 2.
            discounted_price = 2 * book_price * discount1.
            total = total + discounted_price.
            counts = counts - 2.
          ENDIF.
        WHEN OTHERS.
          discounted_price = counts[1] * book_price.
          total = total + discounted_price.
          counts[1] = 0.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
