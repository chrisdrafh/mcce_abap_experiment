CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS calculate_discounted_price
      IMPORTING
        num_books  TYPE i
      RETURNING
        VALUE(price) TYPE p.

    METHODS find_best_discount
      IMPORTING
        counts TYPE STANDARD TABLE OF i
      RETURNING
        VALUE(best_price) TYPE p.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE STANDARD TABLE OF i INITIAL SIZE 5.

    " Initialize counts
    LOOP AT basket INTO DATA(book).
      counts[book] = counts[book] + 1.
    ENDLOOP.

    total = find_best_discount( counts ).
  ENDMETHOD.

  METHOD find_best_discount.
    DATA: price TYPE p.
    best_price = 0.

    " Work from largest discount to smallest
    WHILE counts IS NOT INITIAL.
      DATA(max_set) = 0.
      DATA(set_price) = 0.

      " Check for 5, 4, 3, 2 book sets
      DO 5 TIMES.
        DATA(book_set_count) = sy-index.
        DATA(valid_set_count) = 0.

        " Calculate possible full sets
        LOOP AT counts INTO DATA(count) WHERE count >= book_set_count.
          valid_set_count = valid_set_count + 1.
        ENDLOOP.

        IF valid_set_count >= book_set_count.
          " Deduct counts and add to price
          price = calculate_discounted_price( book_set_count ).

          " Update counts
          LOOP AT counts ASSIGNING FIELD-SYMBOL(<fs_count>).
            IF <fs_count> >= book_set_count.
              <fs_count> = <fs_count> - book_set_count.
            ENDIF.
          ENDLOOP.

          set_price = set_price + price.
          max_set = book_set_count.
          EXIT.
        ENDIF.
      ENDDO.

      " If no set was found, break the loop
      IF max_set = 0.
        EXIT.
      ENDIF.

      best_price = best_price + set_price.
    ENDWHILE.

    " Add remaining individual books
    LOOP AT counts INTO DATA(single_book_count).
      best_price = best_price + single_book_count * 8.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_discounted_price.
    CASE num_books.
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

ENDCLASS.
