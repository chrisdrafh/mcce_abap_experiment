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
      return_table TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY.

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
    DATA(word_count) = NEW return_table( ).

    LOOP AT words INTO DATA(word).
      DATA(existing_entry) = VALUE #( word_count[ word = to_lower( word ) ] DEFAULT sy-tabix ).
      IF existing_entry IS INITIAL.
        word_count = VALUE #( BASE word_count ( word = to_lower( word ) count = 1 ) ).
      ELSE.
        word_count[ sy-tabix ]-count += 1.
      ENDIF.
    ENDLOOP.

    result = word_count.
  ENDMETHOD.

  METHOD split_into_words.
    DATA(separators) = `[\s,.;:"'?!()-]+`.   " Regular expression for word separators
    words = SPLIT phrase AT separators INTO TABLE.
  ENDMETHOD.

ENDCLASS.
