CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING
        basket            TYPE basket_type
      RETURNING
        VALUE(counts)     TYPE TABLE OF i INDEX TABLE.
    METHODS max_discount_group
      IMPORTING
        counts            TYPE TABLE OF i INDEX TABLE
      RETURNING
        VALUE(max_group)  TYPE i.
    METHODS calculate_group_price
      IMPORTING
        num_books         TYPE i
      RETURNING
        VALUE(group_price) TYPE p LENGTH 8 DECIMALS 2.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          max_group TYPE i,
          group_price TYPE p LENGTH 8 DECIMALS 2.

    counts = count_books( basket ).

    WHILE lines( counts ) > 0.
      max_group = max_discount_group( counts ).
      group_price = calculate_group_price( max_group ).
      total = total + group_price.

      DO max_group TIMES.
        READ TABLE counts INDEX 1.
        counts[ sy-tabix ] = counts[ sy-tabix ] - 1.
        IF counts[ sy-tabix ] = 0.
          DELETE counts INDEX sy-tabix.
        ENDIF.
      ENDDO.

      SORT counts DESCENDING.
    ENDWHILE.
  ENDMETHOD.

  METHOD count_books.
    DATA(book TYPE book_id).

    LOOP AT basket INTO book.
      ADD 1 TO counts[ book ].
    ENDLOOP.

    DELETE counts WHERE table_line IS INITIAL.
    SORT counts DESCENDING.
  ENDMETHOD.

  METHOD max_discount_group.
    max_group = COND #( WHEN lines( counts ) >= 5 THEN 5
                        WHEN lines( counts ) = 4 THEN 4
                        WHEN lines( counts ) = 3 THEN 3
                        WHEN lines( counts ) = 2 THEN 2
                        ELSE 1 ).
  ENDMETHOD.

  METHOD calculate_group_price.
    CASE num_books.
      WHEN 5.
        group_price = 5 * 8 * ( 1 - 0.25 ).
      WHEN 4.
        group_price = 4 * 8 * ( 1 - 0.20 ).
      WHEN 3.
        group_price = 3 * 8 * ( 1 - 0.10 ).
      WHEN 2.
        group_price = 2 * 8 * ( 1 - 0.05 ).
      WHEN 1.
        group_price = 1 * 8.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
