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
    DATA: lt_words TYPE STANDARD TABLE OF string,
          lv_word TYPE string.

    " Regular expression to split input into words, preserving contractions
    SPLIT phrase AT '[^[:alnum:]''-]+' INTO TABLE lt_words.

    " Normalize words to lower case and count occurrences
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ).  " Convert to lower case for case insensitivity
      IF lv_word IS NOT INITIAL.
        READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          " Word not found, add new entry
          APPEND VALUE #( word = lv_word count = 1 ) TO result.
        ELSE.
          " Word found, increment count
          MODIFY result ASSIGNING FIELD-SYMBOL(<fs_result>).
          IF <fs_result> IS ASSIGNED.
            <fs_result>-count = <fs_result>-count + 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
