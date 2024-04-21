CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS group_books
      IMPORTING iv_basket TYPE basket_type
      RETURNING VALUE(rt_groups) TYPE TABLE OF i.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: lt_groups TYPE TABLE OF i,
          lv_price TYPE p LENGTH 5 DECIMALS 2.

    " Group the books to maximize discounts
    lt_groups = group_books( iv_basket = basket ).

    " Calculate the total price based on groups
    LOOP AT lt_groups INTO DATA(lv_group_size).
      CASE lv_group_size.
        WHEN 1.
          lv_price = lv_price + 8.
        WHEN 2.
          lv_price = lv_price + 2 * 8 * 0.95.
        WHEN 3.
          lv_price = lv_price + 3 * 8 * 0.90.
        WHEN 4.
          lv_price = lv_price + 4 * 8 * 0.80.
        WHEN 5.
          lv_price = lv_price + 5 * 8 * 0.75.
      ENDCASE.
    ENDLOOP.

    total = lv_price.
  ENDMETHOD.

  METHOD group_books.
    DATA: lt_count TYPE TABLE OF i VALUE INITIAL.
    FIELD-SYMBOLS: <lv_count> LIKE LINE OF lt_count.

    " Count the books
    LOOP AT iv_basket INTO DATA(lv_book).
      ASSIGN COMPONENT lv_book OF STRUCTURE lt_count TO <lv_count>.
      IF <lv_count> IS ASSIGNED.
        <lv_count> = <lv_count> + 1.
      ELSE.
        APPEND 1 TO lt_count INDEX lv_book.
      ENDIF.
    ENDLOOP.

    " Sort counts in descending order to prioritize larger discounts
    SORT lt_count DESCENDING.

    " Determine the optimal groupings
    WHILE lines( lt_count ) > 0.
      " Find the minimal count (this determines the biggest possible group)
      lv_group_size = lt_count[ lines( lt_count ) ].
      " Append group size
      APPEND lv_group_size TO rt_groups.

      " Reduce counts
      DO lv_group_size TIMES.
        lt_count[ sy-index ] = lt_count[ sy-index ] - 1.
        DELETE lt_count WHERE table_line = 0.
      ENDDO.

      SORT lt_count DESCENDING.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
