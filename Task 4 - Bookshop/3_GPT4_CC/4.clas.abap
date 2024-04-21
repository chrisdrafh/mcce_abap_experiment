CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          price TYPE p LENGTH 5 DECIMALS 2,
          discounts TYPE TABLE OF p LENGTH 5 DECIMALS 2,
          max_group_size TYPE i VALUE 5,
          group_discount TYPE p LENGTH 3 DECIMALS 2,
          group_size TYPE i,
          book_id TYPE book_id,
          discount_rate TYPE p LENGTH 3 DECIMALS 2.

    " Define discount rates
    discounts[1] = 0.00. " No discount for 1 book
    discounts[2] = 0.05. " 5% discount for 2 books
    discounts[3] = 0.10. " 10% discount for 3 books
    discounts[4] = 0.20. " 20% discount for 4 books
    discounts[5] = 0.25. " 25% discount for 5 books

    " Count the number of each book
    LOOP AT basket INTO book_id.
      book_counts[ book_id ] = book_counts[ book_id ] + 1.
    ENDLOOP.

    " Try to form groups from largest to smallest to maximize discount
    WHILE max_group_size > 1.
      group_size = max_group_size.
      WHILE group_size > 0.
        " Check if a group of this size can be formed
        DATA(group_possible) = abap_true.
        LOOP AT book_counts INTO DATA(count) FROM 1 TO group_size.
          IF count < 1.
            group_possible = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF group_possible = abap_true.
          " Form the group and apply discount
          discount_rate = discounts[ group_size ].
          LOOP AT book_counts FROM 1 TO group_size.
            book_counts[ sy-tabix ] = book_counts[ sy-tabix ] - 1.
          ENDLOOP.
          group_discount = 8 * group_size * ( 1 - discount_rate ).
          price = price + group_discount.
        ELSE.
          group_size = group_size - 1.
        ENDIF.
      ENDWHILE.

      max_group_size = max_group_size - 1.
    ENDWHILE.

    " Add full price for any remaining books
    LOOP AT book_counts INTO DATA(count).
      price = price + count * 8.
    ENDLOOP.

    total = price.
  ENDMETHOD.

ENDCLASS.
