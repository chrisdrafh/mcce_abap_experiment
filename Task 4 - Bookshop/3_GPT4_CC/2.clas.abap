CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: 
      book_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8.00,
      discount1 TYPE p LENGTH 3 DECIMALS 2 VALUE 0.95,
      discount2 TYPE p LENGTH 3 DECIMALS 2 VALUE 0.90,
      discount3 TYPE p LENGTH 3 DECIMALS 2 VALUE 0.80,
      discount4 TYPE p LENGTH 3 DECIMALS 2 VALUE 0.75.

    METHODS apply_discount
      IMPORTING 
        num_books TYPE i
      RETURNING VALUE(price) TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_optimal_discount
      IMPORTING 
        book_counts TYPE basket_type
      RETURNING VALUE(best_price) TYPE p LENGTH 5 DECIMALS 2.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = calculate_optimal_discount( basket ).
  ENDMETHOD.

  METHOD apply_discount.
    CASE num_books.
      WHEN 1.
        price = book_price.
      WHEN 2.
        price = book_price * 2 * discount1.
      WHEN 3.
        price = book_price * 3 * discount2.
      WHEN 4.
        price = book_price * 4 * discount3.
      WHEN 5.
        price = book_price * 5 * discount4.
      WHEN OTHERS.
        price = book_price * num_books. " No discount for more than 5 identical books
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_optimal_discount.
    DATA: book_count TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line INITIAL SIZE 5.

    " Count each book ID in the basket
    LOOP AT book_counts INTO DATA(book_id).
      ASSIGN book_count[ book_id ] TO FIELD-SYMBOL(<count>).
      IF <count> IS ASSIGNED.
        <count> = <count> + 1.
      ELSE.
        INSERT VALUE #( table_line = book_id, table_line = 1 ) INTO TABLE book_count.
      ENDIF.
    ENDLOOP.

    " Begin with the largest possible sets and apply discounts
    DATA(current_price) = 0.
    WHILE lines( book_count ) > 0.
      DATA(set_size) = lines( book_count ).
      current_price += apply_discount( set_size ).
      " Reduce the counts for each book
      DELETE book_count INDEX set_size.
    ENDWHILE.

    best_price = current_price.
  ENDMETHOD.

ENDCLASS.
