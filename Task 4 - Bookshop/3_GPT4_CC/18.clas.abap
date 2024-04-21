CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_price TYPE p LENGTH 4 DECIMALS 2 VALUE 8.00.

    METHODS calculate_discounted_price
      IMPORTING
        num_books TYPE i
      RETURNING
        VALUE(discounted_price) TYPE p LENGTH 4 DECIMALS 2.

    METHODS find_min_price
      IMPORTING
        books TYPE basket_type
      RETURNING
        VALUE(min_price) TYPE p LENGTH 4 DECIMALS 2.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = find_min_price( basket ).
  ENDMETHOD.

  METHOD calculate_discounted_price.
    CASE num_books.
      WHEN 2.
        discounted_price = c_price * 2 * ( 1 - 0.05 ).
      WHEN 3.
        discounted_price = c_price * 3 * ( 1 - 0.10 ).
      WHEN 4.
        discounted_price = c_price * 4 * ( 1 - 0.20 ).
      WHEN 5.
        discounted_price = c_price * 5 * ( 1 - 0.25 ).
      WHEN OTHERS.
        discounted_price = c_price * num_books.
    ENDCASE.
  ENDMETHOD.

  METHOD find_min_price.
    DATA: number_of_books TYPE i,
          temp_books      TYPE basket_type,
          current_cost    TYPE p LENGTH 4 DECIMALS 2,
          potential_cost  TYPE p LENGTH 4 DECIMALS 2.

    number_of_books = lines( books ).

    IF number_of_books = 0.
      min_price = 0.
      RETURN.
    ENDIF.

    min_price = c_price * number_of_books. " Max cost without any discount

    DO number_of_books TIMES.
      current_cost = calculate_discounted_price( sy-index ).

      CLEAR temp_books.
      temp_books = books.

      " Remove one of each type for the current set size
      LOOP AT temp_books INTO DATA(book) FROM 1 TO sy-index.
        DELETE ADJACENT DUPLICATES FROM temp_books COMPARING table_line.
      ENDLOOP.

      potential_cost = current_cost + find_min_price( temp_books ).

      IF potential_cost < min_price.
        min_price = potential_cost.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
