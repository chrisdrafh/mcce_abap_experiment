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
          ls_result LIKE LINE OF result,
          lt_count TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    " Use regular expression to split phrase into words, handling contractions appropriately
    SPLIT phrase AT '\s|\W' INTO TABLE lt_words WHEN OTHERS IGNORE.

    " Convert words to lowercase and count occurrences
    LOOP AT lt_words INTO lv_word.
      " Normalize to lowercase
      lv_word = to_lower( lv_word ).
      
      " Increment count in hash table
      IF lv_word IS NOT INITIAL.
        ADD 1 TO lt_count[ lv_word ].
      ENDIF.
    ENDLOOP.

    " Move results from hash table to return table
    LOOP AT lt_count ASSIGNING FIELD-SYMBOL(<fs_count>).
      ls_result-word  = <fs_count>-table_line.
      ls_result-count = <fs_count>.
      INSERT ls_result INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
