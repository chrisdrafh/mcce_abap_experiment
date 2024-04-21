CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF book_count,
             book_id TYPE i,
             count TYPE i,
           END OF book_count.

    TYPES: basket_type TYPE SORTED TABLE OF book_count WITH UNIQUE KEY book_id.

    TYPES: total TYPE p LENGTH 10 DECIMALS 2.

    METHODS: constructor,
             calculate_total
               IMPORTING basket       TYPE basket_type
               RETURNING VALUE(total) TYPE total.

  PRIVATE SECTION.
    TYPES: t_discounts TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA: discounts TYPE t_discounts.

    METHODS: apply_discounts
               IMPORTING basket           TYPE basket_type
               RETURNING VALUE(new_basket) TYPE basket_type
               RETURNING VALUE(sub_total)   TYPE total.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD constructor.
    discounts = VALUE #( ( 1 ) ( 5 ) ( 10 ) ( 20 ) ( 25 ) ).
  ENDMETHOD.

  METHOD calculate_total.
    DATA: current_basket TYPE basket_type,
          remaining_basket TYPE basket_type,
          total_price TYPE total VALUE 0,
          sub_total TYPE total.

    current_basket = basket.

    WHILE lines( current_basket ) > 0.
      remaining_basket = apply_discounts( current_basket = current_basket
                                          sub_total = sub_total ).
      total_price += sub_total.
      current_basket = remaining_basket.
    ENDWHILE.

    total = total_price.
  ENDMETHOD.

  METHOD apply_discounts.
    DATA: max_set_size TYPE i VALUE 5.
    DATA: set_count TYPE i.
    DATA: book_idx TYPE i.
    FIELD-SYMBOLS: <fs_book> TYPE book_count.

    SORT basket DESCENDING BY count.
    CLEAR new_basket.
    CLEAR sub_total.

    DO max_set_size TIMES.
      CLEAR set_count.
      LOOP AT basket ASSIGNING <fs_book> WHERE count > 0.
        <fs_book>-count -= 1.
        set_count += 1.
        IF set_count = sy-index.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF set_count = sy-index.
        sub_total += set_count * 8 * ( 1 - discounts[ sy-index ] / 100 ).
      ELSE.
        " Add the non-discounted books back
        LOOP AT basket ASSIGNING <fs_book>.
          IF <fs_book>-count > 0.
            APPEND VALUE #( book_id = <fs_book>-book_id count = <fs_book>-count ) TO new_basket.
          ENDIF.
        ENDLOOP.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
