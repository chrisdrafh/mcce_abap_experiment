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
          lv_word TYPE string,
          ls_result TYPE return_structure,
          lr_result TYPE REF TO data.

    " Use regular expressions to split the phrase into words
    " Regular expression pattern to match words (accounts for contractions)
    SPLIT phrase AT '[^a-zA-Z0-9\']+' INTO TABLE lt_words.

    " Convert words to lower case to count disregarding case sensitivity
    LOOP AT lt_words INTO lv_word.
      lv_word = to_lower( lv_word ). " Convert word to lower case

      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " If the word exists, increase its count
        MODIFY TABLE result WHERE word = lv_word ASSIGNING FIELD-SYMBOL(<ls_result>).
        IF sy-subrc = 0.
          <ls_result>-count = <ls_result>-count + 1.
        ENDIF.
      ELSE.
        " If the word does not exist, add it with count 1
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
