CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_maximum_discount
      IMPORTING
        basket       TYPE basket_type
      RETURNING
        VALUE(max_discount_sets) TYPE basket_type.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          price       TYPE p,
          discounts   TYPE TABLE OF i INDEX TABLE.

    FIELD-SYMBOLS: <book_count> TYPE i.

    " Count books
    LOOP AT basket ASSIGNING FIELD-SYMBOL(<basket_item>).
      ASSIGN book_counts[ <basket_item> ] TO <book_count>.
      IF <book_count> IS ASSIGNED.
        <book_count> = <book_count> + 1.
      ELSE.
        book_counts[ <basket_item> ] = 1.
      ENDIF.
    ENDLOOP.

    " Prepare discounts in descending order of benefit
    discounts = VALUE #(
      ( 5 )  " 25% for 5 distinct books
      ( 4 )  " 20% for 4 distinct books
      ( 3 )  " 10% for 3 distinct books
      ( 2 )  " 5% for 2 distinct books
    ).

    " Calculate total with maximum discount
    price = 0.
    LOOP AT discounts INTO DATA(discount).
      WHILE get_maximum_discount( book_counts ) >= discount.
        price = price + discount * 8 * ( 1 - SWITCH #( discount WHEN 5 THEN 0.25 WHEN 4 THEN 0.20 WHEN 3 THEN 0.10 WHEN 2 THEN 0.05 ELSE 0 ) ).
        LOOP AT book_counts ASSIGNING <book_count> FROM 1 TO discount.
          <book_count> = <book_count> - 1.
        ENDLOOP.
      ENDWHILE.
    ENDLOOP.

    " Add full price for remaining books
    LOOP AT book_counts ASSIGNING <book_count>.
      price = price + <book_count> * 8.
    ENDLOOP.

    total = price.
  ENDMETHOD.

  METHOD get_maximum_discount.
    " Assuming this method returns the number of distinct book sets that can be formed
    max_discount_sets = 0.
    " Dummy implementation
    " In practice, you would implement this to check the available counts and determine
    " the maximum number of distinct sets that can be formed
  ENDMETHOD.

ENDCLASS.
