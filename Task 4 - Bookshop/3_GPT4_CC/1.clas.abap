CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES: book_id TYPE i.

    TYPES: basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES: total TYPE p LENGTH 3 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS: calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: discounts TYPE TABLE OF p LENGTH 3 DECIMALS 2,
          total_discount TYPE p LENGTH 3 DECIMALS 2,
          books TYPE i,
          distinct_books TYPE i.

    "! Initialize discount rates for different numbers of distinct books
    discounts = VALUE #( ( 0.05 ) ( 0.10 ) ( 0.20 ) ( 0.25 ) ).

    "! Count the total number of books and the distinct books in the basket
    books = lines( basket ).
    distinct_books = lines( VALUE #( FOR book IN basket ( book ) ) ).

    "! Calculate the total discount based on the number of distinct books
    total_discount = discounts[ distinct_books - 1 ] * books * 8.

    total = books * 8 - total_discount.
  ENDMETHOD.

ENDCLASS.
