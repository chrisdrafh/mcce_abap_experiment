CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket               TYPE basket_type
      RETURNING VALUE(book_counts)   TYPE TABLE OF i INDEX TABLE.
    METHODS calculate_discount
      IMPORTING num_books            TYPE i
      RETURNING VALUE(discount_rate) TYPE decfloat16.
    METHODS max_discount_grouping
      IMPORTING book_counts          TYPE TABLE OF i INDEX TABLE
      RETURNING VALUE(max_discount)  TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_books( basket ).
    total = max_discount_grouping( book_counts ).
  ENDMETHOD.

  METHOD count_books.
    FIELD-SYMBOLS: <book> TYPE book_id.
    DATA(book_count) = VALUE TABLE OF i( DEFAULT 5 OF 0 ).

    LOOP AT basket ASSIGNING <book>.
      book_count[ CONV int( <book> ) ] += 1.
    ENDLOOP.

    RETURN VALUE #( FOR i = 1 THEN i + 1 UNTIL i > 5
                       ( book_count[ i ] ) ).
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 2.
        discount_rate = 0.95.
      WHEN 3.
        discount_rate = 0.90.
      WHEN 4.
        discount_rate = 0.80.
      WHEN 5.
        discount_rate = 0.75.
      WHEN OTHERS.
        discount_rate = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD max_discount_grouping.
    DATA(price_per_book) = 8.
    DATA(total_discount) TYPE total.

    DO 5 TIMES.
      DATA(max_group) = sy-index.
      DATA(num_sets) = MIN( TABLE LINE OF book_counts ).

      IF max_group = 0 OR num_sets = 0.
        EXIT.
      ENDIF.

      total_discount += num_sets * max_group * calculate_discount( max_group ) * price_per_book.
      book_counts = VALUE #( FOR j = 1 THEN j + 1 UNTIL j > 5
                             ( book_counts[ j ] - num_sets ) ).
    ENDDO.

    RETURN total_discount.
  ENDMETHOD.

ENDCLASS.
