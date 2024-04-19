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
    CLASS-METHODS:
      normalize_text
        IMPORTING
          !phrase       TYPE string
        RETURNING
          VALUE(normalized_phrase) TYPE string.

ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          lt_result TYPE return_table.

    " Normalize input text and split into words
    DATA(lv_normalized_text) = normalize_text( phrase ).

    " Splitting the text into words based on non-word characters excluding apostrophes
    SPLIT lv_normalized_text AT '\s+|(?<!\p{L})'\|'(?!\p{L})|[^\p{L}\d'\']+|\b' INTO TABLE lt_words.

    " Counting the occurrences
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        lv_word = to_lower( lv_word ).
        READ TABLE lt_result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          MODIFY TABLE lt_result FROM VALUE #( word = lv_word count = lt_result[ word = lv_word ]-count + 1 ).
        ELSE.
          INSERT VALUE #( word = lv_word count = 1 ) INTO TABLE lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  METHOD normalize_text.
    " Normalizing the text to ensure consistent parsing
    normalized_phrase = to_lower( phrase ).
    " Handling Unicode and various whitespace characters
    REPLACE ALL OCCURRENCES OF REGEX '[\s]+' IN normalized_phrase WITH ' '.
  ENDMETHOD.

ENDCLASS.
