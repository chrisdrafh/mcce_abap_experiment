CLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! ID of book to buy from 1 to 5
    TYPES book_id TYPE i.

    TYPES basket_type TYPE SORTED TABLE OF book_id
      WITH NON-UNIQUE KEY table_line.

    TYPES total TYPE p LENGTH 3 DECIMALS 2.

    "! @parameter basket | E.g., buying two copies of the first book and one copy of the second book
    "!                     is equivalent to ( ( 1 ) ( 1 ) ( 2 ) )
    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    DATA: occurrences TYPE TABLE OF i INDEX TABLE,
          book        TYPE i,
          prices      TYPE TABLE OF f WITH EMPTY KEY,
          price       TYPE f,
          remaining   TYPE i,
          i           TYPE i.

    " Initialize the count for each book ID
    DO 5 TIMES.
      APPEND 0 TO occurrences.
    ENDDO.

    " Count occurrences of each book in the basket
    LOOP AT basket INTO book.
      occurrences[book] = occurrences[book] + 1.
    ENDLOOP.

    " Try to group the books to apply discounts
    DO.
      " Identify sets of 5, 4, 3, 2, and calculate the price based on discount levels
      remaining = 0.
      LOOP AT occurrences ASSIGNING FIELD-SYMBOL(<occ>) FROM 1 TO 5.
        remaining = remaining + <occ>.
      ENDLOOP.

      IF remaining >= 5.
        APPEND 5 * 8 * 0.75 TO prices. " 25% discount
        LOOP AT occurrences ASSIGNING <occ> FROM 1 TO 5.
          <occ> = <occ> - 1.
        ENDLOOP.
      ELSEIF remaining >= 4.
        APPEND 4 * 8 * 0.80 TO prices. " 20% discount
        FIND FIRST OCCURRENCE OF 1 IN occurrences ASSIGNING <occ> FROM 1.
        IF sy-subrc = 0.
          LOOP AT occurrences ASSIGNING <occ> FROM 1 TO 4 WHERE <occ> > 0.
            <occ> = <occ> - 1.
          ENDLOOP.
        ENDIF.
      ELSEIF remaining >= 3.
        APPEND 3 * 8 * 0.90 TO prices. " 10% discount
        i = 3.
        LOOP AT occurrences ASSIGNING <occ> WHERE <occ> > 0.
          <occ> = <occ> - 1.
          i = i - 1.
          IF i = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSEIF remaining >= 2.
        APPEND 2 * 8 * 0.95 TO prices. " 5% discount
        i = 2.
        LOOP AT occurrences ASSIGNING <occ> WHERE <occ> > 0.
          <occ> = <occ> - 1.
          i = i - 1.
          IF i = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        " Add price for any remaining single books without discount
        LOOP AT occurrences ASSIGNING <occ> WHERE <occ> > 0.
          APPEND 8 TO prices. " No discount
          <occ> = <occ> - 1.
        ENDLOOP.
      ENDIF.

      " Break loop if no more books to process
      remaining = 0.
      LOOP AT occurrences ASSIGNING <occ>.
        remaining = remaining + <occ>.
      ENDLOOP.
      IF remaining = 0.
        EXIT.
      ENDIF.
    ENDDO.

    " Calculate total price
    price = 0.
    LOOP AT prices INTO DATA(each_price).
      price = price + each_price.
    ENDLOOP.

    total = price.
  ENDMETHOD.

ENDCLASS.
