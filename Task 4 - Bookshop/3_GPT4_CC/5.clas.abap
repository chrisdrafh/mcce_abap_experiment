CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
    METHODS count_books
      IMPORTING basket        TYPE basket_type
      RETURNING VALUE(counts) TYPE TABLE OF i INDEX TABLE.

    METHODS max_discount_set
      CHANGING counts TYPE TABLE OF i INDEX TABLE
      RETURNING VALUE(set_size) TYPE i.

    CONSTANTS: book_price TYPE p LENGTH 8 DECIMALS 2 VALUE 8.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_books( basket ).
    DATA(current_total) TYPE p LENGTH 8 DECIMALS 2.

    WHILE lines( book_counts ) > 0.
      DATA(set_size) = max_discount_set( CHANGING counts = book_counts ).
      CASE set_size.
        WHEN 1.
          current_total += book_price.
        WHEN 2.
          current_total += 2 * book_price * 0.95.
        WHEN 3.
          current_total += 3 * book_price * 0.90.
        WHEN 4.
          current_total += 4 * book_price * 0.80.
        WHEN 5.
          current_total += 5 * book_price * 0.75.
      ENDCASE.
    ENDWHILE.

    total = current_total.
  ENDMETHOD.

  METHOD count_books.
    DATA(counts) = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT basket INTO DATA(book_id).
      counts[ book_id ] += 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD max_discount_set.
    DATA(min_count) TYPE i VALUE 5.
    DO 5 TIMES.
      IF counts[ sy-index ] = 0.
        min_count = sy-index - 1.
        EXIT.
      ELSE.
        counts[ sy-index ] -= 1.
      ENDIF.
    ENDDO.
    set_size = min_count.
  ENDMETHOD.

ENDCLASS.
