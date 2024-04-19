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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          ls_result LIKE LINE OF result.

    " Normalize input and split by punctuation and spaces, keeping contractions intact
    TRANSLATE phrase TO LOWER CASE.
    SPLIT phrase AT `[^a-zA-Z0-9']` INTO TABLE lt_words.

    " Remove empty entries which may result from multiple delimiters
    DELETE lt_words WHERE table_line IS INITIAL.

    " Count each word's occurrences
    LOOP AT lt_words INTO lv_word.
      IF lv_word CS '''.
        " Handle contractions by checking if apostrophes are internal
        IF NOT ( lv_word+0(1) = ''' OR lv_word+sy-fdpos(*)-1(1) = ''' ).
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " Word not yet in the table
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE result.
      ELSE.
        " Word exists, increment count
        MODIFY TABLE result FROM VALUE #( word = lv_word count = result[ word = lv_word ]-count + 1 ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
