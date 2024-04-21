CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING
        basket           TYPE basket_type
      RETURNING
        VALUE(book_count) TYPE TABLE OF i INDEX TABLE.
    METHODS find_max_discount_group
      IMPORTING
        book_count       TYPE TABLE OF i INDEX TABLE
      RETURNING
        VALUE(best_group) TYPE i.
    METHODS update_book_count
      CHANGING
        book_count       TYPE TABLE OF i INDEX TABLE
      RETURNING
        VALUE(is_updated) TYPE abap_bool.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_count TYPE TABLE OF i INDEX TABLE,
          best_group TYPE i,
          discount TYPE p LENGTH 10 DECIMALS 2,
          is_updated TYPE abap_bool.

    book_count = count_books( basket ).

    WHILE lines( book_count ) > 0.
      best_group = find_max_discount_group( book_count ).
      CASE best_group.
        WHEN 5.
          discount = 0.75 * 5 * 8.
        WHEN 4.
          discount = 0.80 * 4 * 8.
        WHEN 3.
          discount = 0.90 * 3 * 8.
        WHEN 2.
          discount = 0.95 * 2 * 8.
        WHEN 1.
          discount = 1 * 1 * 8.
      ENDCASE.
      total += discount.
      is_updated = update_book_count( CHANGING book_count = book_count ).
      IF is_updated = abap_false.
        EXIT.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD count_books.
    DATA: id TYPE i.
    LOOP AT basket INTO id.
      book_count[ id ] = COND #( WHEN book_count[ id ] IS INITIAL THEN 1 ELSE book_count[ id ] + 1 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_max_discount_group.
    best_group = 0.
    DATA(available_books) = lines( book_count ).
    CASE available_books.
      WHEN 5.
        best_group = 5.
      WHEN 4.
        best_group = 4.
      WHEN 3.
        best_group = 3.
      WHEN 2.
        best_group = 2.
      WHEN 1.
        best_group = 1.
    ENDCASE.
  ENDMETHOD.

  METHOD update_book_count.
    is_updated = abap_true.
    DATA(removal_count) = find_max_discount_group( book_count ).
    IF removal_count = 0.
      is_updated = abap_false.
    ELSE.
      DO removal_count TIMES.
        LOOP AT book_count ASSIGNING FIELD-SYMBOL(<count>) WHERE <count> > 0.
          <count> = <count> - 1.
          IF <count> = 0.
            DELETE book_count INDEX sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
