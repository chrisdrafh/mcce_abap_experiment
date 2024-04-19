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
          lt_word_count TYPE STANDARD TABLE OF return_structure WITH KEY word,
          ls_word_count TYPE return_structure,
          lr_regex TYPE REF TO cl_abap_regex.

    " Define regex to split text on whitespace or punctuation except apostrophes
    CREATE OBJECT lr_regex
      EXPORTING
        pattern     = '[\s,!?.:;()-]+'
        ignore_case = abap_true.

    " Use regex to split the phrase into words
    SPLIT phrase AT lr_regex INTO TABLE lt_words.

    " Loop through each word, convert to lowercase and count
    LOOP AT lt_words INTO lv_word.
      " Convert word to lower case to handle case insensitivity
      lv_word = to_lower( lv_word ).

      " Check if the word is already counted
      READ TABLE lt_word_count INTO ls_word_count WITH KEY word = lv_word.
      IF sy-subrc = 0.
        " If found, increment the count
        ls_word_count-count = ls_word_count-count + 1.
      ELSE.
        " If not found, initialize count
        ls_word_count-word = lv_word.
        ls_word_count-count = 1.
      ENDIF.

      " Insert or update the table entry
      INSERT ls_word_count INTO TABLE lt_word_count ASSIGNING FIELD-SYMBOL(<ls_word_count>).
      IF sy-subrc = 0.
        " Entry inserted, do nothing
      ELSE.
        " Entry already exists, modify it
        <ls_word_count>-count = ls_word_count-count.
      ENDIF.
    ENDLOOP.

    " Return the result
    result = lt_word_count.

  ENDMETHOD.

ENDCLASS.
