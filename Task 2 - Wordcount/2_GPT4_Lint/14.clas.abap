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
    CLASS-METHODS: normalize_text
      IMPORTING
        !input TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: normalized_text TYPE string,
          lt_words TYPE TABLE OF string,
          word TYPE string,
          lt_result TYPE return_table.

    normalized_text = normalize_text( phrase ).

    " Split text into words
    SPLIT normalized_text AT '\s|[[:punct:]&&[^'']]' INTO TABLE lt_words.

    " Count occurrences of each word
    LOOP AT lt_words INTO word.
      IF word IS NOT INITIAL.
        READ TABLE lt_result WITH KEY word = word TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          MODIFY lt_result WHERE word = word ASSIGNING FIELD-SYMBOL(<fs_word>).
          <fs_word>-count = <fs_word>-count + 1.
        ELSE.
          APPEND VALUE #( word = word count = 1 ) TO lt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = lt_result.
  ENDMETHOD.

  CLASS-METHOD normalize_text.
    " Normalize input text to lower case and handle special characters
    REPLACE ALL OCCURRENCES OF REGEX '([[:punct:]&&[^'\s]])' IN input WITH ' '.
    result = to_lower( input ).
  ENDMETHOD.

ENDCLASS.
