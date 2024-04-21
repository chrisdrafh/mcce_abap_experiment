CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS group_discount
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE p LENGTH 8 DECIMALS 2.

ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA(book) = basket.
    SORT book BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM book COMPARING table_line.

    " Count occurrences of each book ID
    LOOP AT book INTO DATA(book_id).
      book_counts[ book_id ] = COND #( WHEN book_counts[ book_id ] IS NOT INITIAL THEN book_counts[ book_id ] + 1 ELSE 1 ).
    ENDLOOP.

    total = 0.

    " Loop to form groups and apply discounts
    WHILE lines( book_counts ) > 0.
      " Number of different books
      DATA(num_different_books) = lines( book_counts ).

      " Calculate discount for the current group
      total += group_discount( num_different_books ) * num_different_books.

      " Reduce count of each book and clean up
      LOOP AT book_counts INTO DATA(book_count) REFERENCE.
        book_count->* -= 1.
        IF book_count->* = 0.
          DELETE book_counts WHERE table_line = book_count->table_line.
        ENDIF.
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD group_discount.
    CASE num_books.
      WHEN 1.
        discount = 8.00. " No discount
      WHEN 2.
        discount = 8.00 * 0.95. " 5% discount
      WHEN 3.
        discount = 8.00 * 0.90. " 10% discount
      WHEN 4.
        discount = 8.00 * 0.80. " 20% discount
      WHEN 5.
        discount = 8.00 * 0.75. " 25% discount
      WHEN OTHERS.
        discount = 8.00. " Fallback scenario (shouldn't happen)
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
