CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING num_books TYPE i
      RETURNING VALUE(discount) TYPE p LENGTH 5 DECIMALS 2.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          group_size TYPE i,
          max_group_size TYPE i VALUE 5,
          price_per_book TYPE p LENGTH 6 DECIMALS 2 VALUE 8.

    " Count each book's occurrences
    LOOP AT basket INTO DATA(book).
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

    " Initialize total cost
    total = 0.

    " Try to form groups from largest to smallest
    DO max_group_size TIMES.
      group_size = max_group_size + 1 - sy-index.
      WHILE lines( counts ) >= group_size.
        " Check if a group can be formed
        DATA(group_formed) = abap_true.
        LOOP AT counts INTO DATA(count) FROM 1 TO group_size TRANSPORTING NO FIELDS WHERE count = 0.
          group_formed = abap_false.
          EXIT.
        ENDLOOP.

        IF group_formed = abap_true.
          " Decrease count for each book used in this group
          LOOP AT counts ASSIGNING FIELD-SYMBOL(<count>) FROM 1 TO group_size.
            <count> = <count> - 1.
          ENDLOOP.

          " Add to total the cost of this group with discount applied
          total = total + group_size * price_per_book * ( 1 - calculate_discount( group_size ) ).
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.

  METHOD calculate_discount.
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
