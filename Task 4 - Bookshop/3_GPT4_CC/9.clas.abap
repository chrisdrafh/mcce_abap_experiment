CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 7 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS calculate_group_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE p LENGTH 5 DECIMALS 2.

    METHODS find_optimal_groups
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(best_price) TYPE total.

    CLASS-DATA:
      book_prices TYPE TABLE OF p LENGTH 7 DECIMALS 2 WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_optimal_groups( basket ).
  ENDMETHOD.

  METHOD calculate_group_discount.
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
        discount = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD find_optimal_groups.
    DATA(book_count) = VALUE abap_counter_table( FOR book IN basket GROUP BY book INTO GROUP g SIZE 1 ).
    SORT book_count BY table_line DESCENDING.

    best_price = 0.
    WHILE book_count IS NOT INITIAL.
      DATA(max_group_size) = book_count[ 1 ]-table_line.
      DATA(group_discount) = calculate_group_discount( max_group_size ).

      best_price = best_price + ( max_group_size * 8 * group_discount ).
      DELETE book_count INDEX 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
