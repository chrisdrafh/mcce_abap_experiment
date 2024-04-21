CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING 
        num_books TYPE i
      RETURNING
        VALUE(discount_price) TYPE p LENGTH 5 DECIMALS 2.

    METHODS form_groups
      IMPORTING
        basket TYPE basket_type
      RETURNING
        VALUE(groups) TYPE table of i.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: groups TYPE table of i,
          group_price TYPE p LENGTH 5 DECIMALS 2.

    groups = form_groups( basket ).
    total = 0.

    LOOP AT groups INTO DATA(group_size).
      group_price = calculate_discount( group_size ).
      total = total + group_price.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_discount.
    DATA: book_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8.
    CASE num_books.
      WHEN 1.
        discount_price = book_price.
      WHEN 2.
        discount_price = book_price * num_books * 0.95.
      WHEN 3.
        discount_price = book_price * num_books * 0.90.
      WHEN 4.
        discount_price = book_price * num_books * 0.80.
      WHEN 5.
        discount_price = book_price * num_books * 0.75.
    ENDCASE.
  ENDMETHOD.

  METHOD form_groups.
    DATA: counts TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA: max_group TYPE i VALUE 5.

    LOOP AT basket INTO DATA(book).
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

    CLEAR groups.

    WHILE lines( counts ) > 0.
      DATA(current_group) = 0.
      LOOP AT counts INTO DATA(pair) FROM max_group WHILE current_group < max_group.
        IF pair-table_line > 0.
          counts[ pair-table_line ] = counts[ pair-table_line ] - 1.
          current_group = current_group + 1.
        ENDIF.
      ENDLOOP.
      DELETE counts WHERE table_line = 0.
      IF current_group > 0.
        groups = VALUE #( BASE groups ( current_group ) ).
      ENDIF.
      max_group = current_group - 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
