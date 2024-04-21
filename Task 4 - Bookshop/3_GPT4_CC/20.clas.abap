CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 9 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING
        basket            TYPE basket_type
      RETURNING
        VALUE(book_count) TYPE TABLE OF i INDEX TABLE.

    METHODS calculate_discount
      IMPORTING
        num_books         TYPE i
      RETURNING
        VALUE(discount)   TYPE p LENGTH 5 DECIMALS 2.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_count TYPE TABLE OF i INDEX TABLE,
          group_size TYPE i,
          discount   TYPE p,
          price      TYPE p VALUE 8.

    book_count = count_books( basket ).

    total = 0.
    DO.
      group_size = lines( book_count ).
      IF group_size = 0.
        EXIT.
      ENDIF.

      " Determine the discount based on the group size
      discount = calculate_discount( group_size ).

      " Calculate the total for this group and reduce the book counts
      total = total + group_size * price * ( 1 - discount ).

      " Reduce the counts of books for the group just processed
      LOOP AT book_count ASSIGNING FIELD-SYMBOL(<count>).
        IF <count> > 0.
          <count> = <count> - 1.
        ENDIF.
      ENDLOOP.

      DELETE book_count WHERE table_line = 0.
    ENDDO.

  ENDMETHOD.


  METHOD count_books.
    DATA: temp_count TYPE i.
    FIELD-SYMBOLS: <book_id> LIKE LINE OF basket.

    LOOP AT basket ASSIGNING <book_id>.
      temp_count = sy-tabix.
      book_count[ temp_count ] = book_count[ temp_count ] + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_discount.
    CASE num_books.
      WHEN 5.
        discount = 0.25.
      WHEN 4.
        discount = 0.20.
      WHEN 3.
        discount = 0.10.
      WHEN 2.
        discount = 0.05.
      WHEN OTHERS.
        discount = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
