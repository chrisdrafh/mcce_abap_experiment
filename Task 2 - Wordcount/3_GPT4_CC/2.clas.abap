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
        phrase       TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA(words) = split_into_words( phrase ).
    DATA(word_occurrences) = NEW return_table( ).

    LOOP AT words INTO DATA(word).
      IF word_occurrences[ word ] IS INITIAL.
        word_occurrences[ word ] = VALUE #( word = word count = 1 ).
      ELSE.
        word_occurrences[ word ]-count += 1.
      ENDIF.
    ENDLOOP.

    result = word_occurrences.
  ENDMETHOD.

  METHOD split_into_words.
    " Splitting the input string into words, treating certain punctuations as separators
    DATA(lv_phrase) = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[^A-Za-z0-9'']+' IN lv_phrase WITH space.
    SPLIT lv_phrase AT space INTO TABLE words.
  ENDMETHOD.

ENDCLASS.
