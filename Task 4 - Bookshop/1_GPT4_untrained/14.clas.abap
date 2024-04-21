CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 6 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS calculate_discount
      IMPORTING num_books TYPE i
      RETURNING VALUE(discount) TYPE f.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          groups TYPE TABLE OF i INDEX TABLE,
          book_count TYPE i,
          price_per_book TYPE f VALUE 8,
          group_size TYPE i,
          max_group_size TYPE i VALUE 5,
          i TYPE i,
          discount TYPE f.

    FIELD-SYMBOLS: <count> TYPE i.

    " Count each book occurrence
    LOOP AT basket INTO DATA(book).
      ASSIGN counts[ book ] TO <count>.
      IF <count> IS ASSIGNED.
        <count> = <count> + 1.
      ELSE.
        INSERT 1 INTO counts INDEX book.
      ENDIF.
    ENDLOOP.

    " Attempt to form groups starting from largest possible group
    DO max_group_size TIMES.
      group_size = max_group_size - sy-index + 1.

      WHILE group_size <= lines( counts ).
        " Check if a group of 'group_size' can be formed
        book_count = 0.
        LOOP AT counts INTO DATA(count) FROM 1.
          IF count >= 1.
            book_count = book_count + 1.
          ENDIF.
          IF book_count = group_size.
            " Form a group
            LOOP AT counts REFERENCE INTO DATA(count_ref) FROM 1 FOR group_size.
              IF count_ref->* >= 1.
                count_ref->* = count_ref->* - 1.
              ENDIF.
            ENDLOOP.
            APPEND group_size TO groups.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDWHILE.

      IF book_count < group_size.
        CONTINUE.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    " Calculate total price with discounts
    LOOP AT groups INTO group_size.
      discount = calculate_discount( group_size ).
      total = total + group_size * price_per_book * ( 1 - discount ).
    ENDLOOP.
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
        discount = 0.0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
