CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS apply_discount
      IMPORTING
        num_books   TYPE i
      RETURNING
        VALUE(price) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          max_group_size TYPE i VALUE 5,
          remaining TYPE i,
          group_size TYPE i,
          discount_price TYPE total.

    " Count occurrences of each book
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Apply maximum possible discount by trying to form the largest groups of different books
    total = 0.
    WHILE lines( book_counts ) > 0.
      group_size = lines( book_counts ).
      IF group_size > max_group_size.
        group_size = max_group_size.
      ENDIF.

      " Calculate price for this group
      discount_price = apply_discount( group_size ).

      " Add to total
      total = total + discount_price.

      " Decrease count for each book
      LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<count>).
        IF <count> > 0.
          <count> = <count> - 1.
          IF <count> = 0.
            DELETE book_counts INDEX sy-tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.

  METHOD apply_discount.
    CONSTANTS: base_price TYPE total VALUE 8.

    CASE num_books.
      WHEN 1.
        price = base_price.
      WHEN 2.
        price = num_books * base_price * 0.95.
      WHEN 3.
        price = num_books * base_price * 0.90.
      WHEN 4.
        price = num_books * base_price * 0.80.
      WHEN 5.
        price = num_books * base_price * 0.75.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
