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
    CONSTANTS: c_price TYPE p VALUE 8.
    CONSTANTS: c_discounts TYPE STANDARD TABLE OF p WITH DEFAULT KEY INITIAL SIZE 5 COMPONENTS 
      ( 0, 0.05, 0.1, 0.2, 0.25 ).
    METHODS optimize_discounts
      IMPORTING
        counts      TYPE STANDARD TABLE OF i
      RETURNING
        VALUE(price) TYPE p.
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE STANDARD TABLE OF i INDEX TABLE,
          idx         TYPE i,
          group_count TYPE i.

    " Initialize book counts
    book_counts = VALUE #( ( 5 ) OF i ). " Maximum of 5 different books

    " Count the number of each book in the basket
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Optimizing the discount by creating maximum possible groups of unique books
    total = optimize_discounts( book_counts ).
  ENDMETHOD.

  METHOD optimize_discounts.
    DATA: max_group TYPE i VALUE 5,
          possible_groups TYPE i,
          i TYPE i.

    " Repeat until no more books are available to group
    WHILE max_group > 0.
      possible_groups = book_counts[ 1 ].
      " Find the smallest count of books for the potential group
      DO max_group TIMES.
        i = sy-index.
        IF book_counts[ i ] < possible_groups.
          possible_groups = book_counts[ i ].
        ENDIF.
      ENDDO.

      " Apply discount for the group found
      price = price + possible_groups * max_group * c_price * ( 1 - c_discounts[ max_group - 1 ] ).

      " Reduce the count of books used
      DO max_group TIMES.
        i = sy-index.
        book_counts[ i ] = book_counts[ i ] - possible_groups.
      ENDDO.

      " Attempt next smallest group size
      max_group = max_group - 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
