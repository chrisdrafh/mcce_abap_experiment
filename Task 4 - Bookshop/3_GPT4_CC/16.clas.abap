CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING
        basket              TYPE basket_type
      RETURNING VALUE(book_counts) TYPE TABLE OF i INDEX TABLE.
    
    METHODS calculate_group_price
      IMPORTING
        num_different_books TYPE i
      RETURNING VALUE(price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_books( basket ).
    DATA(max_discount_price) TYPE total.

    WHILE lines( book_counts ) > 0.
      DATA(max_books) = lines( book_counts ).
      max_discount_price = max_discount_price + calculate_group_price( max_books ).

      " Decrement the counts for the set just calculated
      LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<count>) FROM 1 FOR max_books.
        <count> = <count> - 1.
      ENDLOOP.

      " Remove any books that now have a count of 0
      DELETE book_counts WHERE table_line = 0.
    ENDWHILE.

    total = max_discount_price.
  ENDMETHOD.

  METHOD count_books.
    " Count how many times each book ID appears in the basket
    DATA(result) = VALUE #( FOR book IN basket ( book = 1 ) GROUP BY book INTO group SIZE group ).
    book_counts = VALUE #( FOR group IN result ( group-size ) ).
  ENDMETHOD.

  METHOD calculate_group_price.
    CASE num_different_books.
      WHEN 1.
        price = 8.0.
      WHEN 2.
        price = 2 * 8 * 0.95.
      WHEN 3.
        price = 3 * 8 * 0.90.
      WHEN 4.
        price = 4 * 8 * 0.80.
      WHEN 5.
        price = 5 * 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
