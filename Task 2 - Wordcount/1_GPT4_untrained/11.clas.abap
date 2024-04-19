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
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result LIKE LINE OF result.

    " Normalize input phrase to lower case and replace punctuations
    phrase = to_lower( phrase ).
    REPLACE ALL OCCURRENCES OF REGEX '[\.,;:\!\?"\[\]\(\)\{\}]' IN phrase WITH ' '.
    SPLIT phrase AT space INTO TABLE lt_words.

    " Process each word and count occurrences
    LOOP AT lt_words INTO lv_word.
      " Skip empty strings that might result from multiple delimiters
      IF lv_word IS NOT INITIAL.
        " Check if the word is already counted
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " Increment count
          READ TABLE result INTO ls_result WITH KEY word = lv_word.
          ls_result-count = ls_result-count + 1.
          MODIFY result FROM ls_result.
        ELSE.
          " Initialize count
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE result.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
