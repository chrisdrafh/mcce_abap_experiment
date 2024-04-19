CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: 
      c_price TYPE p LENGTH 10 DECIMALS 2 VALUE '8.00'.

    METHODS count_books
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(book_counts) TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS get_discount
      IMPORTING number_of_books TYPE i
      RETURNING VALUE(discount) TYPE p LENGTH 10 DECIMALS 2.

ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(book_counts) = count_books( basket ).
    DATA(group_total) TYPE p LENGTH 10 DECIMALS 2.

    WHILE lines( book_counts ) > 0.
      DATA(max_group) = lines( book_counts ).
      DATA(discount) = get_discount( max_group ).

      group_total = max_group * c_price * ( 1 - discount ).
      total += group_total.

      " Reduce the count for each book in the group
      LOOP AT book_counts INTO DATA(book_count) REFERENCE INTO DATA(book_ref).
        IF book_ref->* > 0.
          book_ref->* -= 1.
        ENDIF.
      ENDLOOP.

      DELETE book_counts WHERE table_line = 0.
    ENDWHILE.

  ENDMETHOD.

  METHOD count_books.
    DATA: counts TYPE STANDARD TABLE OF i WITH DEFAULT KEY INITIAL SIZE 5.
    FIELD-SYMBOLS: <count> LIKE LINE OF counts.

    " Initialize counts for each book ID from 1 to 5
    DO 5 TIMES.
      APPEND 0 TO counts.
    ENDDO.

    " Count each book in the basket
    LOOP AT basket INTO DATA(book).
      ASSIGN counts[ book ] TO <count>.
      IF <count> IS ASSIGNED.
        <count> += 1.
      ENDIF.
    ENDLOOP.

    " Sort the counts in descending order to maximize discount application
    SORT counts DESCENDING.
    book_counts = counts.
  ENDMETHOD.

  METHOD get_discount.
    CASE number_of_books.
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
