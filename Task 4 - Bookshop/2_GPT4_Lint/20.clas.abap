CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_book_count,
             book_id TYPE i,
             count   TYPE i,
           END OF ty_book_count.

    TYPES: basket_type TYPE SORTED TABLE OF ty_book_count
                            WITH UNIQUE KEY book_id.

    TYPES: total TYPE p LENGTH 8 DECIMALS 2.

    METHODS: calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: calculate_discounted_price
      IMPORTING 
        num_books TYPE i
      RETURNING VALUE(price) TYPE p.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: sorted_basket TYPE basket_type,
          temp_count    TYPE i.

    " Sorting the basket to prioritize books with lesser count for maximum discount optimization
    sorted_basket = VALUE #( FOR <fs_basket> IN basket
                              ( book_id = <fs_basket>-book_id
                                count   = <fs_basket>-count )
                             SORT BY count ASCENDING ).

    total = 0.

    WHILE lines( sorted_basket ) > 0.
      CLEAR temp_count.

      " Determine the maximum set of unique books we can form
      DO lines( sorted_basket ) TIMES.
        temp_count = temp_count + 1.
        sorted_basket[ 1 ]-count = sorted_basket[ 1 ]-count - 1.
        IF sorted_basket[ 1 ]-count = 0.
          DELETE sorted_basket INDEX 1.
        ELSE.
          sorted_basket = VALUE #( BASE sorted_basket
                                    FOR <fs_basket> IN sorted_basket
                                    ( book_id = <fs_basket>-book_id
                                      count   = <fs_basket>-count )
                                    SORT BY count ASCENDING ).
        ENDIF.
        IF temp_count = lines( sorted_basket ) OR temp_count = 5.
          EXIT.
        ENDIF.
      ENDDO.

      total = total + calculate_discounted_price( temp_count ).
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate_discounted_price.
    CONSTANTS: book_price TYPE p LENGTH 8 DECIMALS 2 VALUE '8.00'.

    CASE num_books.
      WHEN 1.
        price = book_price.
      WHEN 2.
        price = num_books * book_price * ( 1 - 0.05 ). " 5% discount
      WHEN 3.
        price = num_books * book_price * ( 1 - 0.10 ). " 10% discount
      WHEN 4.
        price = num_books * book_price * ( 1 - 0.20 ). " 20% discount
      WHEN 5.
        price = num_books * book_price * ( 1 - 0.25 ). " 25% discount
      WHEN OTHERS.
        price = num_books * book_price.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
