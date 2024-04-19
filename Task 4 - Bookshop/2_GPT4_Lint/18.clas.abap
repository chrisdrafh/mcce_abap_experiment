CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
    CONSTANTS: c_price TYPE p LENGTH 3 DECIMALS 2 VALUE '8.00'.

    METHODS count_books
      IMPORTING basket        TYPE basket_type
      RETURNING VALUE(counts) TYPE TABLE OF i INDEX TABLE.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          groups TYPE i OCCURS 5,
          idx    TYPE i,
          subtotal TYPE p LENGTH 3 DECIMALS 2,
          discount TYPE p LENGTH 3 DECIMALS 2.

    counts = count_books( basket ).

    " Calculate largest groups possible to maximize discount
    DO 5 TIMES.
      IF counts[ sy-index ] >= 1.
        groups[ sy-index ] = COUNT( val = counts WHERE table_line >= sy-index ).
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    " Calculate total with maximum discount
    DO.
      CLEAR: idx, subtotal, discount.
      idx = sy-index.

      IF groups[ idx ] >= 1.
        CASE idx.
          WHEN 5.
            discount = 0.75. " 25% discount
          WHEN 4.
            discount = 0.80. " 20% discount
          WHEN 3.
            discount = 0.90. " 10% discount
          WHEN 2.
            discount = 0.95. " 5% discount
          WHEN 1.
            discount = 1.00. " No discount
        ENDCASE.

        subtotal = groups[ idx ] * idx * c_price * discount.
        total = total + subtotal.

        " Reduce counts
        DO idx TIMES.
          counts[ sy-index ] = counts[ sy-index ] - groups[ idx ].
        ENDDO.

        groups[ idx ] = groups[ idx ] - 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD count_books.
    DATA(book TYPE i).

    " Initialize counts for books 1 to 5
    counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).

    " Count each book occurrence
    LOOP AT basket INTO book.
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
