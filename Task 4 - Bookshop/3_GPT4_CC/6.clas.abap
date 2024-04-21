CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 8 DECIMALS 2,
      t_counts TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_discount
      IMPORTING 
        num_books TYPE i 
      RETURNING 
        VALUE(discount) TYPE decfloat16.

    METHODS form_groups
      IMPORTING
        counts    TYPE t_counts
      RETURNING
        VALUE(groups) TYPE t_counts.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = VALUE t_counts( FOR <book> IN basket ( book = count( val = <book> ) ) ).
    DATA(groups) = form_groups( book_counts ).
    DATA(book_price) = 8. " Fixed price per book
    total = 0.

    LOOP AT groups INTO DATA(group_size).
      DATA(discount) = get_discount( group_size ).
      total = total + group_size * book_price * ( 1 - discount ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_discount.
    CASE num_books.
      WHEN 1.
        discount = 0.
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

  METHOD form_groups.
    " Initialize groups
    groups = VALUE #( ( 5 = count( val = 5 WHERE table_line < 6 AND table_line > 0 ) )
                      ( 4 = count( val = 4 WHERE table_line < 5 AND table_line > 0 ) )
                      ( 3 = count( val = 3 WHERE table_line < 4 AND table_line > 0 ) )
                      ( 2 = count( val = 2 WHERE table_line < 3 AND table_line > 0 ) )
                      ( 1 = count( val = 1 WHERE table_line < 2 AND table_line > 0 ) ) ).
  ENDMETHOD.

ENDCLASS.
