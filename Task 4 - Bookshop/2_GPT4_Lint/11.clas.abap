CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 10 DECIMALS 2,
           book_count TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS: constructor,
             calculate_total
               IMPORTING basket       TYPE basket_type
               RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: calculate_discounted_price
               IMPORTING num_books TYPE i
               RETURNING VALUE(price) TYPE p LENGTH 10 DECIMALS 2,
             find_optimal_discount
               IMPORTING basket       TYPE basket_type
                         depth        TYPE i
               RETURNING VALUE(total) TYPE total.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD calculate_total.
    total = find_optimal_discount( basket = basket depth = 1 ).
  ENDMETHOD.

  METHOD calculate_discounted_price.
    CASE num_books.
      WHEN 1.
        price = 8.0.
      WHEN 2.
        price = 2 * 8 * 0.95.
      WHEN 3.
        price = 3 * 8 * 0.90.
      WHEN 4.
        price = 4 * 8 * 0.80.
      WHEN 5.
        price = 5 * 8 * 0.75.
      WHEN OTHERS.
        price = num_books * 8.
    ENDCASE.
  ENDMETHOD.

  METHOD find_optimal_discount.
    DATA: max_discount TYPE total,
          temp_basket TYPE basket_type,
          current_discount TYPE total,
          num_books TYPE i.

    " Recursive base case: basket is empty
    IF basket IS INITIAL.
      RETURN 0.
    ENDIF.

    max_discount = cl_abap_math=>ceil( lines( basket ) * 8 ).
    LOOP AT basket INTO DATA(book).
      CLEAR: temp_basket.
      temp_basket = basket.
      DELETE temp_basket WHERE table_line = book.
      num_books = lines( basket ) - lines( temp_basket ).

      current_discount = calculate_discounted_price( num_books ) +
                         find_optimal_discount( basket = temp_basket depth = depth + 1 ).

      IF current_discount < max_discount.
        max_discount = current_discount.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM temp_basket COMPARING table_line.
      IF temp_basket IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    RETURN max_discount.
  ENDMETHOD.

ENDCLASS.
