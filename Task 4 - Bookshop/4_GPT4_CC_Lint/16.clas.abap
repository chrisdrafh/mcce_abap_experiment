CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 7 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(counts) TYPE STANDARD TABLE OF i INDEX TABLE.
    METHODS max_discount
      IMPORTING num_books    TYPE i
      RETURNING VALUE(discount) TYPE f.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE STANDARD TABLE OF i INDEX TABLE,
          group_size TYPE i,
          discount_rate TYPE f,
          prices TYPE STANDARD TABLE OF f INDEX TABLE.

    counts = count_books( basket ).
    WHILE lines( counts ) > 0.
      group_size = lines( counts ).
      discount_rate = max_discount( group_size ).

      " Calculate price for this group
      APPEND group_size * ( 1 - discount_rate ) * 8 TO prices.

      " Reduce count for each book
      DELETE counts INDEX lines( counts ).
      LOOP AT counts ASSIGNING FIELD-SYMBOL(<count>).
        <count> = <count> - 1.
        IF <count> = 0.
          DELETE counts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    total = REDUCE total( INIT sum TYPE f FOR price IN prices NEXT sum = sum + price ).

  ENDMETHOD.

  METHOD count_books.
    DATA(book_counter) = VALUE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line( ).
    LOOP AT basket INTO DATA(book).
      book_counter[ book ] = COND #( WHEN line_exists( book_counter[ book ] )
                                     THEN book_counter[ book ] + 1
                                     ELSE 1 ).
    ENDLOOP.
    counts = VALUE #( FOR val IN book_counter ( val ) ).
  ENDMETHOD.

  METHOD max_discount.
    CASE num_books.
      WHEN 5.
        discount = 0.25.
      WHEN 4.
        discount = 0.20.
      WHEN 3.
        discount = 0.10.
      WHEN 2.
        discount = 0.05.
      WHEN 1.
        discount = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
