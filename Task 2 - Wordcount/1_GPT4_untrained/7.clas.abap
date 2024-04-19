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
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word  TYPE string,
          ls_result TYPE return_structure,
          lt_result TYPE return_table.

    " Replace punctuations that separate words with spaces
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9A-Za-z''']' IN phrase WITH ' '.
    
    " Convert to lower case to make the count case insensitive
    phrase = to_lower( phrase ).

    " Split words into a table
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Loop over the words to count their occurrences
    LOOP AT lt_words INTO lv_word.
      " Skip empty words resulting from consecutive spaces or punctuations
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Word already in result, increment count
          MODIFY lt_result FROM VALUE #( word = lv_word count = ls_result-count + 1 ) TRANSPORTING count WHERE word = lv_word.
        ELSE.
          " New word, add to result
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Return the results
    result = lt_result.

  ENDMETHOD.
ENDCLASS.
