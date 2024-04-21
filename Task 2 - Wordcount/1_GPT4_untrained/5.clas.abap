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
      return_table TYPE HASHED TABLE OF return_structure WITH UNIQUE KEY word.
    
    METHODS count_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(result) TYPE return_table .

ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word TYPE string,
          ls_result TYPE return_structure.

    " Using regular expressions to split the input phrase into words
    SPLIT phrase AT '\W+' INTO TABLE lt_words
         WHERE NOT ( value CA '\d' OR value CA '\p' OR value IS INITIAL ).

    " Normalize case and count occurrences
    LOOP AT lt_words INTO lv_word.
      " Convert to lowercase to ensure case insensitivity
      lv_word = to_lower( lv_word ).

      " Skip any entry that isn't a valid word or is a number
      IF lv_word CA 'abcdefghijklmnopqrstuvwxyz' OR lv_word CO '0123456789'.
        READ TABLE result WITH KEY word = lv_word INTO ls_result.
        IF sy-subrc <> 0.
          ls_result-word = lv_word.
          ls_result-count = 1.
          INSERT ls_result INTO TABLE result.
        ELSE.
          ls_result-count = ls_result-count + 1.
          MODIFY result FROM ls_result TRANSPORTING count WHERE word = lv_word.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
