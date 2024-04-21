CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word  TYPE string,
          ls_result TYPE return_structure.

    " Use Regular Expression to split the phrase into words
    SPLIT phrase AT '[^a-zA-Z0-9'\']+' INTO TABLE lt_words.

    " Convert each word to lower case to ensure case insensitivity
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word exists, increase count
        MODIFY result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 )
          TRANSPORTING count WHERE word = lv_word.
      ELSE.
        " If word doesn't exist, add to result table
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
