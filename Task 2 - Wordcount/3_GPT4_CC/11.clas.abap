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
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lv_phrase TYPE string,
          lt_words TYPE TABLE OF string,
          lt_result TYPE return_table,
          lv_word TYPE string,
          lv_word_index TYPE sy-tabix.

    " Normalize the input: convert to lower case
    lv_phrase = to_lower( phrase ).

    " Tokenize the input using regex
    SPLIT lv_phrase AT '[\s,:;!?."()]+|\[\]' INTO TABLE lt_words.

    " Count words
    LOOP AT lt_words INTO lv_word.
      " Ignore empty strings which can occur due to multiple delimiters
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS INTO lv_word_index.
        IF sy-subrc = 0.
          " Word exists, increment the count
          lt_result[ sy-tabix ]-count = lt_result[ sy-tabix ]-count + 1.
        ELSE.
          " New word, add to the table
          APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Set result table
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
