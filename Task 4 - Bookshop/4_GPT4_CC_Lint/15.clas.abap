CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 8 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    METHODS apply_discount
      IMPORTING num_books TYPE i
      RETURNING VALUE(price) TYPE p.

    CLASS-METHODS create_frequency_table
      IMPORTING basket TYPE basket_type
      RETURNING VALUE(freq_table) TYPE TABLE OF i.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: freq_table TYPE TABLE OF i,
          group_sizes TYPE TABLE OF i,
          index TYPE i,
          max_group TYPE i,
          subtotal TYPE p.

    freq_table = create_frequency_table( basket ).

    WHILE lines( freq_table ) > 0.
      max_group = lines( freq_table ).
      APPEND max_group TO group_sizes.
      LOOP AT freq_table INTO index FROM lines( freq_table ) DOWNTO 1.
        freq_table[ index ] = freq_table[ index ] - 1.
        IF freq_table[ index ] = 0.
          DELETE freq_table INDEX index.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    SORT group_sizes DESCENDING.
    subtotal = 0.
    LOOP AT group_sizes INTO max_group.
      subtotal = subtotal + apply_discount( max_group ).
    ENDLOOP.

    total = subtotal.
  ENDMETHOD.

  METHOD apply_discount.
    CASE num_books.
      WHEN 1.
        price = 8.
      WHEN 2.
        price = 2 * 8 * 0.95.
      WHEN 3.
        price = 3 * 8 * 0.90.
      WHEN 4.
        price = 4 * 8 * 0.80.
      WHEN 5.
        price = 5 * 8 * 0.75.
    ENDCASE.
  ENDMETHOD.

  CLASS-METHODS create_frequency_table.
    DATA: freq TYPE TABLE OF i WITH DEFAULT KEY INITIAL SIZE 5.

    LOOP AT basket INTO DATA(book_id).
      ASSIGN COMPONENT book_id OF STRUCTURE freq TO FIELD-SYMBOL(<fs_freq>).
      IF sy-subrc = 0.
        <fs_freq> = <fs_freq> + 1.
      ELSE.
        INSERT 1 INTO freq INDEX book_id.
      ENDIF.
    ENDLOOP.

    freq_table = freq.
  ENDMETHOD.

ENDCLASS.
