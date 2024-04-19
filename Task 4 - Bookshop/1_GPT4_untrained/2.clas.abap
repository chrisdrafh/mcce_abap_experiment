CLASS zcl_anagram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_candidate,
             word TYPE string,
           END OF ty_candidate,
           tt_candidates TYPE STANDARD TABLE OF ty_candidate WITH DEFAULT KEY.

    METHODS anagram
      IMPORTINGCLASS zcl_book_store DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: book_id TYPE i,
           basket_type TYPE SORTED TABLE OF book_id WITH NON-UNIQUE KEY table_line,
           total TYPE p LENGTH 7 DECIMALS 2.

    METHODS calculate_total
      IMPORTING basket       TYPE basket_type
      RETURNING VALUE(total) TYPE total.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS compute_discount
      IMPORTING num_books      TYPE i
      RETURNING VALUE(discount) TYPE f.

    METHODS find_best_price
      IMPORTING basket             TYPE basket_type
      RETURNING VALUE(best_price)  TYPE p LENGTH 7 DECIMALS 2.
ENDCLASS.


CLASS zcl_book_store IMPLEMENTATION.

  METHOD calculate_total.
    total = me->find_best_price( basket ).
  ENDMETHOD.

  METHOD compute_discount.
    CASE num_books.
      WHEN 1.
        discount = 1.0. " No discount
      WHEN 2.
        discount = 0.95. " 5% discount
      WHEN 3.
        discount = 0.90. " 10% discount
      WHEN 4.
        discount = 0.80. " 20% discount
      WHEN 5.
        discount = 0.75. " 25% discount
      WHEN OTHERS.
        discount = 1.0.
    ENDCASE.
  ENDMETHOD.

  METHOD find_best_price.
    DATA: max_books TYPE i VALUE 5,
          price TYPE p LENGTH 7 DECIMALS 2,
          prices TYPE TABLE OF p LENGTH 7 DECIMALS 2,
          num_books TYPE i,
          counts TYPE STANDARD TABLE OF i INITIAL SIZE 5,
          i TYPE i,
          book TYPE book_id.

    " Initialize counts of each book
    DO max_books TIMES.
      APPEND 0 TO counts.
    ENDDO.

    " Count the books
    LOOP AT basket INTO book.
      counts[ book ] = counts[ book ] + 1.
    ENDLOOP.

    " Generate all combinations and calculate the prices
    WHILE counts IS NOT INITIAL.
      CLEAR: num_books, price.
      LOOP AT counts INTO num_books FROM 1 TO max_books.
        IF num_books > 0.
          counts[ sy-index ] = counts[ sy-index ] - 1.
          price = price + compute_discount( sy-index ) * 8.
        ENDIF.
      ENDLOOP.
      APPEND price TO prices.
      DELETE counts WHERE table_line = 0.
    ENDWHILE.

    " Find the minimum price
    best_price = REDUCE p( INIT x TYPE p LENGTH 7 DECIMALS 2 FOR <price> IN prices NEXT x = x + <price> ).
  ENDMETHOD.

ENDCLASS.

        input         TYPE string
        candidates    TYPE tt_candidates
      RETURNING
        VALUE(result) TYPE tt_candidates.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS is_anagram
      IMPORTING
        word1 TYPE string
        word2 TYPE string
      RETURNING
        VALUE(is_anag) TYPE abap_bool.
ENDCLASS.


CLASS zcl_anagram IMPLEMENTATION.
  METHOD anagram.
    CLEAR result.
    LOOP AT candidates INTO DATA(candidate).
      IF is_anagram( input, candidate-word ).
        APPEND candidate TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_anagram.
    is_anag = abap_false.

    DATA(lv_word1) = to_lower( word1 ).
    DATA(lv_word2) = to_lower( word2 ).

    " A word is not its own anagram
    IF lv_word1 = lv_word2.
      RETURN.
    ENDIF.

    " Sort characters
    SORT VALUE #( FOR <char> IN lv_word1 ( <char> ) ) INTO lv_word1.
    SORT VALUE #( FOR <char> IN lv_word2 ( <char> ) ) INTO lv_word2.

    IF lv_word1 = lv_word2.
      is_anag = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
