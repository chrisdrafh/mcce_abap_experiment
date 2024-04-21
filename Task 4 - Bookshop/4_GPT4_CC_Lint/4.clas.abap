CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! ID of book to buy from 1 to 5
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_max_discount
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(total) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = calculate_max_discount( basket ).
  ENDMETHOD.

  METHOD calculate_max_discount.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          set_sizes TYPE TABLE OF i,
          price_per_book TYPE p LENGTH 8 DECIMALS 2 VALUE 8,
          discount TYPE p LENGTH 3 DECIMALS 2,
          discounted_price TYPE p LENGTH 8 DECIMALS 2.

    FIELD-SYMBOLS: <count> TYPE i.

    " Count occurrences of each book
    LOOP AT basket INTO DATA(book).
      ASSIGN book_counts[ book ] TO <count>.
      IF <count> IS ASSIGNED.
        <count> = <count> + 1.
      ELSE.
        INSERT 1 INTO book_counts INDEX book.
      ENDIF.
    ENDLOOP.

    " Calculate the best discount by trying different set sizes
    WHILE lines( book_counts ) > 0.
      CLEAR set_sizes.
      DO 5 TIMES.
        IF lines( book_counts ) >= sy-index AND book_counts[ sy-index ] > 0.
          APPEND sy-index TO set_sizes.
          book_counts[ sy-index ] = book_counts[ sy-index ] - 1.
        ENDIF.
      ENDDO.

      SORT set_sizes DESCENDING.
      IF lines( set_sizes ) = 5.
        discount = 0.25.
      ELSEIF lines( set_sizes ) = 4.
        discount = 0.20.
      ELSEIF lines( set_sizes ) = 3.
        discount = 0.10.
      ELSEIF lines( set_sizes ) = 2.
        discount = 0.05.
      ELSE.
        discount = 0.00.
      ENDIF.

      discounted_price = price_per_book * ( 1 - discount ) * lines( set_sizes ).
      total = total + discounted_price.

      DELETE book_counts WHERE table_line = 0.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
