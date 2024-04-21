CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS find_best_price
      IMPORTING
        books          TYPE basket_type
      RETURNING
        VALUE(best_price) TYPE total.

    METHODS calculate_discount
      IMPORTING
        num_books      TYPE i
      RETURNING
        VALUE(discount_price) TYPE p.

    METHODS count_books
      IMPORTING
        books          TYPE basket_type
      RETURNING
        VALUE(counts)  TYPE TABLE OF i.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_best_price( basket ).
  ENDMETHOD.

  METHOD find_best_price.
    DATA: book_counts TYPE TABLE OF i,
          groups TYPE TABLE OF i,
          current_price TYPE p,
          temp_price TYPE p.

    book_counts = count_books( books ).

    " Attempt to make groups of 5 down to 1
    LOOP AT book_counts INTO DATA(book_count).
      WHILE book_count > 0.
        DATA(group_size) = 0.
        " Check if a group can be formed
        LOOP AT book_counts INTO DATA(count) FROM 1.
          IF count > 0.
            group_size = group_size + 1.
            book_counts[ sy-tabix ] = book_counts[ sy-tabix ] - 1.
          ENDIF.
          IF group_size = 5.
            EXIT.
          ENDIF.
        ENDLOOP.

        " Calculate price for this group
        temp_price = calculate_discount( group_size ) * group_size.
        current_price = current_price + temp_price.

        " Update book counts
        book_counts = book_counts.
      ENDWHILE.
    ENDLOOP.

    best_price = current_price.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 1.
        discount_price = 8.00.
      WHEN 2.
        discount_price = 8.00 * 0.95.
      WHEN 3.
        discount_price = 8.00 * 0.90.
      WHEN 4.
        discount_price = 8.00 * 0.80.
      WHEN 5.
        discount_price = 8.00 * 0.75.
    ENDCASE.
  ENDMETHOD.

  METHOD count_books.
    " Count number of each book in the basket
    counts = VALUE #( ( FOR i = 1 THEN i + 1 UNTIL i > 5
                         LET count = REDUCE i( INIT x = 0 FOR book IN books WHERE ( book = i ) NEXT x = x + 1 )
                         IN count ) ).
  ENDMETHOD.

ENDCLASS.
