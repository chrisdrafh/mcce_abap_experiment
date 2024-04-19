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
    METHODS split_string_to_words
      IMPORTING
        !phrase   TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lt_result TYPE return_table.

    " Split the phrase into words
    lt_words = split_string_to_words( phrase ).

    " Count each word
    LOOP AT lt_words INTO DATA(lv_word).
      DATA(ls_word_index) = sy-tabix.
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_result ASSIGNING FIELD-SYMBOL(<ls_result>) WITH KEY word = lv_word.
        IF sy-subrc = 0.
          " Increment count if word exists
          <ls_result>-count = <ls_result>-count + 1.
        ELSE.
          " Add new word with count 1 if it does not exist
          APPEND VALUE #( word = lv_word count = 1 ) TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD split_string_to_words.
    " Use regex to find words, considering contractions as part of words
    DATA(lv_pattern) TYPE string VALUE `[^a-zA-Z0-9']+`.

    " Replace punctuations and spaces with a space
    REPLACE ALL OCCURRENCES OF REGEX lv_pattern IN phrase WITH space.
    " Normalize to lowercase to handle case insensitivity
    TRANSLATE phrase TO LOWER CASE.
    " Split the string into words
    SPLIT phrase AT space INTO TABLE words.

    " Remove empty entries, if any
    DELETE words WHERE table_line IS INITIAL.
  ENDMETHOD.
ENDCLASS.
