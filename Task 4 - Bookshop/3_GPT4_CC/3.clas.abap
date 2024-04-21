CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_discount_rate
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discount) TYPE decfloat16.
    METHODS get_price_after_discount
      IMPORTING
        num_books  TYPE i
        base_price TYPE p
      RETURNING
        VALUE(price) TYPE p.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INITIAL SIZE 5 WITH KEY table_line COMPONENTS 1,
          group_size TYPE i,
          discounted_price TYPE p.

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Calculate price by forming groups with maximum discount
    total = 0.
    DO.
      group_size = 0.
      " Attempt to form the largest group of different books
      LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<count>) WHERE <count> > 0.
        <count> = <count> - 1.
        group_size = group_size + 1.
      ENDLOOP.

      " If no group could be formed, exit
      IF group_size = 0.
        EXIT.
      ENDIF.

      " Get discounted price for the group and add to total
      discounted_price = get_price_after_discount( group_size, 8 ). " 8 is the base price per book
      total = total + discounted_price * group_size.
    ENDDO.
  ENDMETHOD.

  METHOD get_discount_rate.
    CASE num_books.
      WHEN 2.
        discount = 0.05.
      WHEN 3.
        discount = 0.10.
      WHEN 4.
        discount = 0.20.
      WHEN 5.
        discount = 0.25.
      WHEN OTHERS.
        discount = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD get_price_after_discount.
    price = base_price * ( 1 - get_discount_rate( num_books ) ).
  ENDMETHOD.

ENDCLASS.
