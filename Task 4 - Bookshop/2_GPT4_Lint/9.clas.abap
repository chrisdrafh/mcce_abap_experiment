CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF book_count,
             book_id TYPE i,
             count   TYPE i,
           END OF book_count.

    TYPES basket_type TYPE SORTED TABLE OF book_count
      WITH NON-UNIQUE KEY book_id.

    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_optimal_group
      IMPORTING
        basket TYPE basket_type
      EXPORTING
        group_count TYPE i
      CHANGING
        remaining_basket TYPE basket_type.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: remaining_basket TYPE basket_type,
          group_count      TYPE i,
          discount         TYPE f,
          price            TYPE p LENGTH 8 DECIMALS 2.

    remaining_basket = basket.

    WHILE lines( remaining_basket ) > 0.
      CLEAR: group_count.

      find_optimal_group(
        EXPORTING basket = remaining_basket
        IMPORTING group_count = group_count
        CHANGING remaining_basket = remaining_basket ).

      CASE group_count.
        WHEN 5.
          discount = 0.75.
        WHEN 4.
          discount = 0.80.
        WHEN 3.
          discount = 0.90.
        WHEN 2.
          discount = 0.95.
        WHEN 1.
          discount = 1.
      ENDCASE.

      price = price + group_count * 8 * discount.
    ENDWHILE.

    total = price.
  ENDMETHOD.

  METHOD find_optimal_group.
    DATA: max_group_size TYPE i VALUE 5.
    DATA: idx TYPE i.

    SORT basket BY book_id ASCENDING.

    group_count = 0.

    DO max_group_size TIMES.
      READ TABLE basket INTO DATA(book) INDEX 1.
      IF sy-subrc = 0 AND book-count > 0.
        group_count = group_count + 1.
        book-count = book-count - 1.
        MODIFY basket FROM book INDEX 1.
        IF book-count = 0.
          DELETE basket INDEX 1.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    remaining_basket = basket.
  ENDMETHOD.

ENDCLASS.
