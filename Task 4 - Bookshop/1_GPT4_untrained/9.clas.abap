CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 5 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_books
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(counts) TYPE TABLE OF i INDEX TABLE.

    METHODS max_discount_group
      CHANGING  counts        TYPE TABLE OF i INDEX TABLE
      RETURNING VALUE(group_size) TYPE i.

    CONSTANTS: book_price TYPE p LENGTH 5 DECIMALS 2 VALUE 8.

ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: counts TYPE TABLE OF i INDEX TABLE,
          group_size TYPE i,
          price TYPE p LENGTH 5 DECIMALS 2,
          discount TYPE p LENGTH 5 DECIMALS 2.

    counts = count_books( basket ).

    WHILE lines( counts ) > 0.
      group_size = max_discount_group( CHANGING counts = counts ).
      CASE group_size.
        WHEN 2.
          discount = 0.05.
        WHEN 3.
          discount = 0.10.
        WHEN 4.
          discount = 0.20.
        WHEN 5.
          discount = 0.25.
        WHEN OTHERS.
          discount = 0.
      ENDCASE.

      price = price + group_size * book_price * ( 1 - discount ).
    ENDWHILE.

    total = price.

  ENDMETHOD.

  METHOD count_books.
    FIELD-SYMBOLS: <book> LIKE LINE OF counts.
    LOOP AT basket INTO DATA(book).
      ASSIGN counts[ book - 1 ] TO <book>.
      IF <book> IS ASSIGNED.
        <book> = <book> + 1.
      ELSE.
        INSERT 1 INTO TABLE counts INDEX book - 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD max_discount_group.
    DATA(max_books) = lines( counts ).
    DATA(idx) = 1.

    WHILE idx <= max_books.
      IF counts[ idx ] = 0.
        DELETE counts INDEX idx.
        max_books = max_books - 1.
      ELSE.
        counts[ idx ] = counts[ idx ] - 1.
        group_size = group_size + 1.
      ENDIF.
      idx = idx + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
