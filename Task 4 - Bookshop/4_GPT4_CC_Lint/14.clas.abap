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
ENDCLASS.



CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE SORTED TABLE OF i INDEX TABLE
            WITH NON-UNIQUE KEY table_line,
          discounts TYPE TABLE OF decfloat16,
          n TYPE i,
          max_group_size TYPE i VALUE 5,
          book_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8,
          discount_price TYPE p LENGTH 5 DECIMALS 2.

    " Initialize discounts
    discounts = VALUE #( ( 0 ) ( 0.05 ) ( 0.10 ) ( 0.20 ) ( 0.25 ) ).

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      READ TABLE counts WITH KEY table_line = book TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        counts[ sy-tabix ] = counts[ sy-tabix ] + 1.
      ELSE.
        APPEND 1 TO counts.
      ENDIF.
    ENDLOOP.

    " Sort book counts descending to try to use the biggest sets first
    SORT counts DESCENDING.

    total = 0.

    " Attempt to form groups with maximum discount first
    WHILE counts IS NOT INITIAL.
      n = counts[ 1 ].

      IF n > 0.
        " Determine the size of the group to form
        DO max_group_size TIMES.
          IF sy-index > lines( counts ) OR counts[ sy-index ] < 1.
            EXIT.
          ENDIF.
        ENDDO.

        " Update count after forming a group
        discount_price = book_price * ( 1 - discounts[ sy-index - 1 ] ) * ( sy-index - 1 ).
        total += discount_price.

        " Update the remaining counts of each book
        DO sy-index - 1 TIMES.
          counts[ sy-index ] = counts[ sy-index ] - 1.
          IF counts[ sy-index ] = 0.
            DELETE counts INDEX sy-index.
            sy-index = sy-index - 1.
          ENDIF.
        ENDDO.
      ELSE.
        EXIT.
      ENDIF.

      " Resort to handle next largest possible group
      SORT counts DESCENDING.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
