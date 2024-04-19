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
    METHODS normalize_text
      IMPORTING
        !text   TYPE string
      RETURNING
        VALUE(normalized_text) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string.

    DATA(lv_normalized_text) = normalize_text( phrase ).
    " Split the normalized text into words
    SPLIT lv_normalized_text AT '\s+' INTO TABLE lt_words.

    " Count occurrences
    LOOP AT lt_words INTO lv_word.
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " If word already exists, increment the count
          MODIFY result ASSIGNING FIELD-SYMBOL(<fs_word>) WHERE word = lv_word.
          <fs_word>-count = <fs_word>-count + 1.
        ELSE.
          " If word does not exist, add it with count 1
          APPEND VALUE #( word = lv_word count = 1 ) TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_text.
    normalized_text = to_lower( text ).
    " Replace punctuation marks (excluding apostrophes) with space
    REPLACE ALL OCCURRENCES OF REGEX '[^\w'']' IN normalized_text WITH ' '.
  ENDMETHOD.

ENDCLASS.
