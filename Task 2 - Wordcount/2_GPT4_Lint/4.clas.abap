CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
          lv_regex TYPE string,
          lt_result TYPE return_table.

    " Define regex to split text into words (handling contractions as whole words)
    lv_regex = `(?i)\b[\w']+\b`.

    " Use regex to split phrase into words
    FIND ALL OCCURRENCES OF REGEX lv_regex IN phrase MATCHING RESULTS INTO lt_words.

    " Convert each word to lowercase and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ). " Case insensitive
      READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If word exists in the result table, increase count
        MODIFY TABLE lt_result FROM VALUE #( word = lv_word count = lt_result[ word = lv_word ]-count + 1 ).
      ELSE.
        " If word does not exist, add new entry with count 1
        INSERT VALUE #( word = lv_word count = 1 ) INTO TABLE lt_result.
      ENDIF.
    ENDLOOP.

    " Return the table of words with their counts
    result = lt_result.
  ENDMETHOD.
ENDCLASS.
