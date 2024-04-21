CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: 
      book_id TYPE i,
      basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
      total TYPE p LENGTH 10 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: book_counts TYPE TABLE OF i INDEX TABLE,
          group_counts TYPE TABLE OF i INDEX TABLE,
          discount TYPE p LENGTH 5 DECIMALS 2 VALUE 8,
          discounts TYPE TABLE OF p LENGTH 5 DECIMALS 2 VALUE '8 7.6 7.2 6.4 6' INDEX TABLE,
          max_group_size TYPE i VALUE 5,
          group_size TYPE i,
          book TYPE i,
          n TYPE i.

    " Count occurrences of each book
    LOOP AT basket INTO book.
      book_counts[ book ] = book_counts[ book ] + 1.
    ENDLOOP.

    " Try to form groups starting from the largest size to smallest to maximize discounts
    DO max_group_size TIMES.
      group_size = max_group_size + 1 - sy-index.
      WHILE group_size <= lines( book_counts ).
        n = group_size.
        LOOP AT book_counts TRANSPORTING NO FIELDS WHERE table_line >= 1.
          n = n - 1.
          IF n = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF n = 0.
          " Form a group
          LOOP AT book_counts REFERENCE INTO DATA(book_ref) WHERE table_line >= 1.
            book_ref->* = book_ref->* - 1.
          ENDLOOP.
          group_counts[ group_size ] = group_counts[ group_size ] + 1.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDWHILE.
    ENDDO.

    " Calculate total based on groups formed
    total = 0.
    LOOP AT group_counts INTO n FROM 1.
      IF n > 0.
        total = total + n * discounts[ sy-tabix ] * group_counts[ sy-tabix ].
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
