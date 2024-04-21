CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
    METHODS calculate_discount_group
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount_price) TYPE p LENGTH 3 DECIMALS 2.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA(book) = VALUE book_id( ).

    " Count each book type in the basket
    LOOP AT basket INTO book.
      ASSIGN counts[ book ] TO FIELD-SYMBOL(<count>).
      IF sy-subrc = 0.
        <count> = <count> + 1.
      ELSE.
        INSERT VALUE #( table_line = book ) INTO TABLE counts.
      ENDIF.
    ENDLOOP.

    " Maximize discount by trying to form the largest possible unique sets
    DATA: total_price TYPE p LENGTH 3 DECIMALS 2 VALUE 0.
    WHILE lines( counts ) > 0.
      DATA(unique_set_count) = lines( counts ).
      IF unique_set_count > 5.
        unique_set_count = 5.
      ENDIF.

      " Deduct the counted books from inventory and calculate price
      DATA(set_price) = calculate_discount_group( unique_set_count ).
      total_price += set_price * unique_set_count.

      " Reduce count for each book in the set
      LOOP AT counts ASSIGNING FIELD-SYMBOL(<cnt>).
        <cnt> = <cnt> - 1.
        IF <cnt> = 0.
          DELETE counts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    total = total_price.
  ENDMETHOD.

  METHOD calculate_discount_group.
    CASE num_books.
      WHEN 1.
        discount_price = 8.
      WHEN 2.
        discount_price = 8 * ( 1 - 0.05 ).
      WHEN 3.
        discount_price = 8 * ( 1 - 0.10 ).
      WHEN 4.
        discount_price = 8 * ( 1 - 0.20 ).
      WHEN 5.
        discount_price = 8 * ( 1 - 0.25 ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
