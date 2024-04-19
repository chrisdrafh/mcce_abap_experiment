CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS book_price TYPE p LENGTH 5 DECIMALS 2 VALUE '8.00'.
    TYPES discount TYPE p LENGTH 3 DECIMALS 2.
    TYPES discount_table TYPE HASHED TABLE OF discount WITH UNIQUE KEY table_line.
    DATA discounts TYPE discount_table.

    METHODS initialize_discounts.
    METHODS maximize_discounts
      IMPORTING
        quantities TYPE basket_type
      RETURNING VALUE(max_total) TYPE total.
    METHODS calculate_group_price
      IMPORTING
        num_books TYPE i
      RETURNING VALUE(price) TYPE p LENGTH 5 DECIMALS 2.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: quantities TYPE basket_type.
    LOOP AT basket INTO DATA(book).
      DATA(existing) = sy-tabix.
      READ TABLE quantities WITH TABLE LINE book TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND book TO quantities.
      ELSE.
        quantities[ existing ] = quantities[ existing ] + 1.
      ENDIF.
    ENDLOOP.
    total = maximize_discounts( quantities ).
  ENDMETHOD.

  METHOD initialize_discounts.
    discounts = VALUE #(
      ( 1 = 0 )
      ( 2 = 0.05 )  " 5% discount
      ( 3 = 0.10 )  " 10% discount
      ( 4 = 0.20 )  " 20% discount
      ( 5 = 0.25 )  " 25% discount
    ).
  ENDMETHOD.

  METHOD maximize_discounts.
    DATA(total_price) = 0.
    " Logic to find the optimal distribution of books for maximum discount goes here
    " This would typically involve a recursive or dynamic programming approach
    " to maximize the discount given the basket configuration.

    " Example logic (naive and needs optimal solution):
    DO 5 TIMES.
      IF lines( quantities ) >= 5.
        total_price = total_price + calculate_group_price( 5 ).
        DELETE ADJACENT DUPLICATES FROM quantities COMPARING table_line.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDDO.
    max_total = total_price.
  ENDMETHOD.

  METHOD calculate_group_price.
    price = num_books * book_price * ( 1 - discounts[ num_books ] ).
  ENDMETHOD.

ENDCLASS.
