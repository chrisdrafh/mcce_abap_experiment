CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          group_size TYPE i,
          max_group_size TYPE i VALUE 5,
          discount TYPE f,
          price TYPE p LENGTH 8 DECIMALS 2 VALUE 8,
          discounted_price TYPE p LENGTH 8 DECIMALS 2.

    FIELD-SYMBOLS: <count> LIKE LINE OF counts.

    " Count occurrences of each book in the basket
    LOOP AT basket ASSIGNING FIELD-SYMBOL(<book>).
      ASSIGN counts[ <book> ] TO <count>.
      IF <count> IS ASSIGNED.
        <count> = <count> + 1.
      ELSE.
        INSERT 1 INTO counts INDEX <book>.
      ENDIF.
    ENDLOOP.

    " Sorting counts to get the maximum potential for largest groups
    SORT counts DESCENDING.

    " Calculate the total price
    total = 0.
    WHILE lines( counts ) > 0.
      " Determine the maximum group size possible
      group_size = lines( counts ).

      IF group_size > max_group_size.
        group_size = max_group_size.
      ENDIF.

      " Apply discount based on group size
      CASE group_size.
        WHEN 1.
          discount = 0.0.
        WHEN 2.
          discount = 0.05.
        WHEN 3.
          discount = 0.1.
        WHEN 4.
          discount = 0.2.
        WHEN 5.
          discount = 0.25.
      ENDCASE.

      " Calculate discounted price for the group
      discounted_price = price * group_size * ( 1 - discount ).
      total = total + discounted_price.

      " Decrease the count of each book processed
      DO group_size TIMES.
        counts[ sy-index ] = counts[ sy-index ] - 1.
        IF counts[ sy-index ] = 0.
          DELETE counts INDEX sy-index.
        ENDIF.
      ENDDO.

      " Sort again to prioritize larger groups if possible
      SORT counts DESCENDING.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
