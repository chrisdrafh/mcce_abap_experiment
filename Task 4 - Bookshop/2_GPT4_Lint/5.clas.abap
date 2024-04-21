CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_discount
      IMPORTING num_books TYPE i
      RETURNING VALUE(discount) TYPE p LENGTH 3 DECIMALS 2.

    METHODS update_counts
      CHANGING ct_counts TYPE TABLE OF i
              iv_group_size TYPE i.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INITIAL SIZE 5,
          group_size TYPE i,
          discount TYPE p,
          sub_total TYPE p.

    FIELD-SYMBOLS: <fs_count> TYPE i.

    " Initialize counts
    counts = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ).

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      ADD 1 TO counts[ book ].
    ENDLOOP.

    " Calculate total cost
    total = 0.

    DO.
      group_size = 0.

      " Determine the size of the current group
      LOOP AT counts ASSIGNING <fs_count> WHERE <fs_count> > 0.
        ADD 1 TO group_size.
        SUBTRACT 1 FROM <fs_count>.
      ENDLOOP.

      " Exit when no more groups can be formed
      IF group_size = 0.
        EXIT.
      ENDIF.

      " Get discount for the current group size
      discount = get_discount( group_size ).

      " Calculate subtotal for this group
      sub_total = group_size * 8 * ( 1 - discount ).
      ADD sub_total TO total.
    ENDDO.

  ENDMETHOD.

  METHOD get_discount.
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

ENDCLASS.
