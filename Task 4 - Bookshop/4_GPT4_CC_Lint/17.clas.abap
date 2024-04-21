CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES book_id TYPE i.
    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.
    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_sets
      IMPORTING
        iv_books TYPE basket_type
      RETURNING VALUE(rv_sets) TYPE STANDARD TABLE OF i
      WITH NON-UNIQUE KEY DEFAULT KEY.
ENDCLASS.

CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA(lv_total) TYPE p LENGTH 3 DECIMALS 2.
    DATA(lt_sets) TYPE STANDARD TABLE OF i.

    lt_sets = count_sets( basket ).
    LOOP AT lt_sets INTO DATA(lv_set_size).
      CASE lv_set_size.
        WHEN 1.
          lv_total += 1 * 8.
        WHEN 2.
          lv_total += 2 * 8 * 0.95.
        WHEN 3.
          lv_total += 3 * 8 * 0.90.
        WHEN 4.
          lv_total += 4 * 8 * 0.80.
        WHEN 5.
          lv_total += 5 * 8 * 0.75.
      ENDCASE.
    ENDLOOP.

    total = lv_total.
  ENDMETHOD.

  METHOD count_sets.
    DATA(lt_count) TYPE STANDARD TABLE OF i WITH NON-UNIQUE KEY DEFAULT KEY.
    DATA(lv_max_books) TYPE i VALUE 5.
    FIELD-SYMBOLS: <lv_count> TYPE i.

    LOOP AT iv_books INTO DATA(lv_book).
      READ TABLE lt_count WITH KEY table_line = lv_book ASSIGNING <lv_count>.
      IF sy-subrc = 0.
        <lv_count> = <lv_count> + 1.
      ELSE.
        APPEND 1 TO lt_count INDEX lv_book.
      ENDIF.
    ENDLOOP.

    CLEAR rv_sets.
    DO lv_max_books TIMES.
      DATA(lv_min_count) TYPE i VALUE lv_max_books.
      LOOP AT lt_count INTO <lv_count>.
        IF <lv_count> < lv_min_count.
          lv_min_count = <lv_count>.
        ENDIF.
      ENDLOOP.

      IF lv_min_count > 0.
        APPEND lv_max_books TO rv_sets lv_min_count TIMES.
        LOOP AT lt_count ASSIGNING <lv_count>.
          <lv_count> = <lv_count> - lv_min_count.
        ENDLOOP.
      ENDIF.

      lv_max_books = lv_max_books - 1.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
