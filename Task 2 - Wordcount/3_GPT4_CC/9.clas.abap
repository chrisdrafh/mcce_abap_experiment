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
    METHODS split_phrase_into_words
      IMPORTING
        !phrase TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
    METHODS normalize_word
      IMPORTING
        !word   TYPE string
      RETURNING
        VALUE(normalized_word) TYPE string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA(words) = split_phrase_into_words( phrase ).
    DATA(word_occurrences) = NEW return_table( ).

    LOOP AT words INTO DATA(word).
      DATA(normalized_word) = normalize_word( word ).
      READ TABLE word_occurrences ASSIGNING FIELD-SYMBOL(<fs_word_occurrence>) WITH KEY word = normalized_word.
      IF sy-subrc = 0.
        ADD 1 TO <fs_word_occurrence>-count.
      ELSE.
        APPEND VALUE #( word = normalized_word count = 1 ) TO word_occurrences.
      ENDIF.
    ENDLOOP.

    result = word_occurrences.
  ENDMETHOD.

  METHOD split_phrase_into_words.
    " Split the phrase into words considering contractions as whole words
    " and handling multiple forms of punctuation and whitespace
    SPLIT phrase AT '[\s\.,;:!?"]+' INTO TABLE words.
  ENDMETHOD.

  METHOD normalize_word.
    " Convert the word to lower case for case insensitivity
    normalized_word = to_lower( word ).
  ENDMETHOD.

ENDCLASS.
