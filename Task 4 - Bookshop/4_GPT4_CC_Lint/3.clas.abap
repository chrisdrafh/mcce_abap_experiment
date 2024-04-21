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

    "! Calculate total price for the given basket of books.
    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE SORTED TABLE OF i INDEX TABLE WITH UNIQUE KEY table_line,
          price       TYPE p LENGTH 3 DECIMALS 2 VALUE 8,
          discount    TYPE p LENGTH 3 DECIMALS 2.

    FIELD-SYMBOLS: <count> TYPE i.

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      ASSIGN book_counts[ book ] TO <count>.
      IF sy-subrc = 0.
        ADD 1 TO <count>.
      ELSE.
        book_counts[ book ] = 1.
      ENDIF.
    ENDLOOP.

    " Calculate total price with optimal discount
    total = 0.
    WHILE lines( book_counts ) > 0.
      CASE lines( book_counts ).
        WHEN 5.
          discount = 0.75. " 25% discount
          total = total + 5 * price * discount.
          DELETE ADJACENT DUPLICATES FROM book_counts COMPARING table_line.
          DELETE book_counts INDEX 1.
        WHEN 4.
          discount = 0.8. " 20% discount
          total = total + 4 * price * discount.
          DELETE ADJACENT DUPLICATES FROM book_counts COMPARING table_line.
          DELETE book_counts INDEX 1.
        WHEN 3.
          discount = 0.9. " 10% discount
          total = total + 3 * price * discount.
          DELETE ADJACENT DUPLICATES FROM book_counts COMPARING table_line.
          DELETE book_counts INDEX 1.
        WHEN 2.
          discount = 0.95. " 5% discount
          total = total + 2 * price * discount.
          DELETE ADJACENT DUPLICATES FROM book_counts COMPARING table_line.
          DELETE book_counts INDEX 1.
        WHEN 1.
          total = total + price.
          CLEAR book_counts.
      ENDCASE.
      DELETE book_counts WHERE table_line = 0.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
