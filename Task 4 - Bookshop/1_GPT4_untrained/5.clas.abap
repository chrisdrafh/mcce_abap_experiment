CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS create_max_discount_sets
      IMPORTING
        book_counts TYPE STANDARD TABLE OF i
      RETURNING
        VALUE(discounted_price) TYPE p.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INITIAL SIZE 5,
          idx TYPE i,
          discounted_price TYPE p.

    " Initialize book counts
    DO 5 TIMES.
      APPEND 0 TO book_counts.
    ENDDO.

    " Count each book
    LOOP AT basket INTO DATA(book).
      idx = book - 1.
      book_counts[idx] = book_counts[idx] + 1.
    ENDLOOP.

    " Calculate the total cost with maximum discount
    discounted_price = create_max_discount_sets( book_counts = book_counts ).

    total = discounted_price.
  ENDMETHOD.

  METHOD create_max_discount_sets.
    DATA: possible_discounts TYPE TABLE OF i WITH NON-UNIQUE DEFAULT KEY,
          base_price TYPE p VALUE 8.

    " Define discounts
    possible_discounts = VALUE #(
      ( 5 )  " 25% for 5 different books
      ( 4 )  " 20% for 4 different books
      ( 3 )  " 10% for 3 different books
      ( 2 )  " 5% for 2 different books
      ( 1 )  " 0% for 1 book
    ).

    SORT book_counts DESCENDING.

    LOOP AT possible_discounts INTO DATA(discount_set).
      DATA: num_sets TYPE i VALUE 0.

      " Find how many sets of this size can be formed
      WHILE book_counts[discount_set] >= discount_set.
        num_sets = num_sets + 1.
        book_counts = REDUCE #( INIT x = book_counts FOR j = 1 UNTIL j > discount_set
                                NEXT x[j] = x[j] - 1 ).
      ENDWHILE.

      " Apply discounts based on the number of sets
      CASE discount_set.
        WHEN 5.
          discounted_price = discounted_price + num_sets * discount_set * base_price * 0.75.
        WHEN 4.
          discounted_price = discounted_price + num_sets * discount_set * base_price * 0.8.
        WHEN 3.
          discounted_price = discounted_price + num_sets * discount_set * base_price * 0.9.
        WHEN 2.
          discounted_price = discounted_price + num_sets * discount_set * base_price * 0.95.
        WHEN 1.
          discounted_price = discounted_price + num_sets * discount_set * base_price.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
