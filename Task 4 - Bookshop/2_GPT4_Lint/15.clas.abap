CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    TYPES: tt_counts TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS count_books
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(counts) TYPE tt_counts.

    METHODS max_discount
      IMPORTING counts TYPE tt_counts
      RETURNING VALUE(max_total) TYPE total.

ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE tt_counts,
          max_total TYPE total.

    counts = count_books( basket ).
    max_total = max_discount( counts ).
    total = max_total.
  ENDMETHOD.

  METHOD count_books.
    FIELD-SYMBOLS: <fs_book> LIKE LINE OF basket.
    counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).
    LOOP AT basket ASSIGNING <fs_book>.
      counts[ <fs_book> - 1 ] += 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD max_discount.
    DATA: sets TYPE tt_counts,
          prices TYPE tt_counts VALUE #( 800 760 720 640 600 ),
          n TYPE i,
          remaining TYPE i,
          index TYPE i,
          temp_total TYPE p LENGTH 8 DECIMALS 2.

    " Initialize total calculation
    max_total = 0.

    " Strategy: Try to form groups from largest to smallest for maximizing discount
    DO 5 TIMES.
      n = 5 - sy-index.

      " Count how many full sets of size n can be formed
      remaining = counts[ sy-index ].
      DO n TIMES.
        index = sy-index MOD 5.
        IF counts[ index ] < remaining.
          remaining = counts[ index ].
        ENDIF.
      ENDDO.

      " Apply discount for full sets formed
      IF remaining > 0.
        max_total += remaining * prices[ n - 1 ] * n / 1000. " price per book in a set of size n
        " Reduce the counts
        DO n TIMES.
          index = ( sy-index + sy-index MOD n ) MOD 5.
          counts[ index ] -= remaining.
        ENDDO.
      ENDIF.
    ENDDO.

    " Add full price for remaining books
    LOOP AT counts INTO temp_total.
      max_total += temp_total * 800 / 1000. " remaining books at full price
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
