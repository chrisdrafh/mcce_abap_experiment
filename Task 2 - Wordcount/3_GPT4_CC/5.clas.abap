CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF return_structure,
        word  TYPE string,
        count TYPE i,
      END OF return_structure,
      return_table TYPE STANDARD TABLE OF return_structure WITH KEY word.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS split_into_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(words)  TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA(words) = split_into_words( phrase ).
    DATA(word_counts) = VALUE return_table( ).

    LOOP AT words INTO DATA(word).
      DATA(lv_word_lc) = to_lower( word ).
      " Update the count for each word, case-insensitively
      IF line_exists( word_counts[ word = lv_word_lc ] ).
        word_counts[ word = lv_word_lc ]-count += 1.
      ELSE.
        INSERT VALUE #( word = lv_word_lc count = 1 ) INTO TABLE word_counts.
      ENDIF.
    ENDLOOP.

    result = word_counts.
  ENDMETHOD.

  METHOD split_into_words.
    " Regular expression to split input into words. It captures sequences of alphabets or numbers
    " and treats contractions as a part of the word.
    DATA(re) TYPE string VALUE `[a-zA-Z0-9]+('[a-zA-Z0-9]+)?`.
    words = NEW cl_abap_matcher( pattern = re )->find_all( text = phrase ).

    " Normalize words to be case insensitive and handle contraction consistency.
    LOOP AT words ASSIGNING FIELD-SYMBOL(<word>).
      <word> = to_lower( <word> ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
