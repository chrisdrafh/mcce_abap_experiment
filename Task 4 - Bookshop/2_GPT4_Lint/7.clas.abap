CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 7 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_distinct_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(counts) TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: distinct_counts TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    distinct_counts = count_distinct_books( basket ).
    total = 0.

    WHILE lines( distinct_counts ) > 0.
      CASE lines( distinct_counts ).
        WHEN 5.
          total = total + 5 * 8 * 0.75.  " 25% discount
          REMOVE INDEX FROM distinct_counts FOR 5 TIMES.
        WHEN 4.
          total = total + 4 * 8 * 0.80.  " 20% discount
          REMOVE INDEX FROM distinct_counts FOR 4 TIMES.
        WHEN 3.
          total = total + 3 * 8 * 0.90.  " 10% discount
          REMOVE INDEX FROM distinct_counts FOR 3 TIMES.
        WHEN 2.
          total = total + 2 * 8 * 0.95.  " 5% discount
          REMOVE INDEX FROM distinct_counts FOR 2 TIMES.
        WHEN 1.
          total = total + 8.            " no discount
          REMOVE INDEX FROM distinct_counts FOR 1 TIME.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
      DESCENDING SORT distinct_counts.
    ENDWHILE.

  ENDMETHOD.

  METHOD count_distinct_books.
    DATA(book_count) = VALUE itab( FOR book IN basket ( book ) ).
    counts = VALUE #( ( LINES OF VALUE #( FOR book IN book_count GROUP BY book ( book ) ) ) GROUP BY GROUP SIZE ).
  ENDMETHOD.

ENDCLASS.
