CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE INITIAL SIZE 5,
          discount_rates TYPE TABLE OF decfloat16 INDEX TABLE INITIAL SIZE 5,
          price_per_book TYPE decfloat16 VALUE 8,
          max_books TYPE i VALUE 5,
          subtotal TYPE decfloat16,
          group_size TYPE i.

    " Initialize book counts
    book_counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).

    " Fill book counts
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Initialize discount rates for different group sizes
    discount_rates = VALUE #( ( 1.0 ) ( 0.95 ) ( 0.90 ) ( 0.80 ) ( 0.75 ) ).

    " Calculate total by forming optimal groups
    WHILE max_books > 0.
      " Determine the size of the current group
      group_size = 0.
      DO max_books TIMES.
        IF book_counts[ sy-index ] > 0.
          book_counts[ sy-index ] = book_counts[ sy-index ] - 1.
          group_size = group_size + 1.
        ENDIF.
      ENDDO.

      " Calculate subtotal for this group
      IF group_size > 0.
        subtotal = subtotal + group_size * price_per_book * discount_rates[ group_size ].
      ELSE.
        " No more groups can be formed
        EXIT.
      ENDIF.
    ENDWHILE.

    " Set the returned total
    total = subtotal.

  ENDMETHOD.

ENDCLASS.
