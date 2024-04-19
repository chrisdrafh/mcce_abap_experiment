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
      return_table TYPE STANDARD TABLE OF return_structure WITH DEFAULT KEY.
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      normalize_text
        IMPORTING
          i_text  TYPE string
        RETURNING
          VALUE(r_text) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string,
          lv_cleaned_text TYPE string.

    " Normalize the input text
    lv_cleaned_text = normalize_text( phrase ).

    " Split the text into words based on whitespace and punctuation
    SPLIT lv_cleaned_text AT '[^'\w]+' INTO TABLE lt_words.

    " Initialize the result table
    CLEAR: result.

    " Count each word
    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " Word not yet in table, add it with count 1
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ELSE.
        " Word exists, increment count
        READ TABLE result WITH KEY word = lv_word ASSIGNING FIELD-SYMBOL(<fs_word>).
        <fs_word>-count = <fs_word>-count + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  CLASS-METHOD normalize_text.
    " Convert to lowercase and replace some punctuation with spaces, keeping contractions
    r_text = to_lower( i_text ).
    " Replace punctuation that separates words with space, keeping apostrophes in contractions
    REPLACE ALL OCCURRENCES OF REGEX '[!?,.:;"()\-[\]{}<>*&^%$#@=`~|\\]+' IN r_text WITH ' '.
  ENDMETHOD.

ENDCLASS.
