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
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string.

    " Normalize the input to handle case insensitivity
    phrase = to_lower( phrase ).

    " Replace all punctuations that are not apostrophes with space
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN phrase WITH ' '.

    " Split the phrase into words based on spaces
    SPLIT phrase AT ' ' INTO TABLE lt_words.

    " Remove any empty entries which might have been created by multiple spaces
    DELETE lt_words WHERE table_line IS INITIAL.

    " Use a temporary internal table to count occurrences
    TYPES: BEGIN OF ty_count,
             word  TYPE string,
             count TYPE i,
           END OF ty_count.
    DATA: lt_count TYPE STANDARD TABLE OF ty_count WITH DEFAULT KEY.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      " If the word is not empty, proceed to count
      IF lv_word IS NOT INITIAL.
        READ TABLE lt_count WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          " Word not found, insert it with count 1
          INSERT VALUE ty_count( word = lv_word count = 1 ) INTO TABLE lt_count.
        ELSE.
          " Word found, increase count
          MODIFY TABLE lt_count FROM VALUE #( word = lv_word count = sy-tabix + 1 ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Transfer counted data to the returning parameter structure
    LOOP AT lt_count INTO DATA(ls_count).
      INSERT VALUE #( word = ls_count-word count = ls_count-count ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
