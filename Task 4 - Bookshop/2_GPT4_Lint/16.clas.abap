CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          groups TYPE TABLE OF i INDEX TABLE,
          discounts TYPE TABLE OF decfloat16 INDEX TABLE.

    " Initialize discounts
    discounts[1] = 1.   " No discount for single book
    discounts[2] = 0.95 " 5% discount for two different books
    discounts[3] = 0.9  " 10% discount for three different books
    discounts[4] = 0.8  " 20% discount for four different books
    discounts[5] = 0.75 " 25% discount for five different books

    " Count each book
    LOOP AT basket INTO DATA(book).
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Determine the largest set that can be formed repeatedly
    WHILE lines( book_counts ) > 0.
      DATA(set_size) = lines( book_counts ).
      IF set_size > 5.
        set_size = 5.
      ENDIF.

      " Add the group size and apply the corresponding discount
      groups = VALUE #( BASE groups ( set_size ) ).
      LOOP AT book_counts ASSIGNING FIELD-SYMBOL(<book_count>) WHERE <book_count> > 0.
        <book_count> = <book_count> - 1.
        DELETE book_counts WHERE <fs_book_count> = 0.
      ENDLOOP.
    ENDWHILE.

    " Calculate total price using discounts
    total = 0.
    LOOP AT groups INTO DATA(group_size).
      total = total + group_size * discounts[ group_size ] * 8.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
