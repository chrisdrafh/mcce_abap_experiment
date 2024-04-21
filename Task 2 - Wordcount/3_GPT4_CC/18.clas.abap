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
    METHODS split_into_words
      IMPORTING
        !phrase       TYPE string
      RETURNING
        VALUE(words)  TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          lv_word  TYPE string.
    FIELD-SYMBOLS: <fs_word> TYPE string.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO lv_word.
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " Word exists, increment count
        READ TABLE result ASSIGNING <fs_word> WITH KEY word = lv_word.
        <fs_word>-count += 1.
      ELSE.
        " New word, add to table
        APPEND VALUE #( word = lv_word count = 1 ) TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_into_words.
    " Normalize phrase to lower case to ensure case insensitivity
    phrase = to_lower( phrase ).

    " Replace punctuation with space, except apostrophes within words
    REPLACE ALL OCCURRENCES OF REGEX '[\.,!?:;-\[\]\(\)"\n\t]+' IN phrase WITH ' '.
    
    " Split phrase into words based on space
    SPLIT phrase AT ' ' INTO TABLE words.
    
    " Remove empty entries which might have been created by multiple spaces
    DELETE words WHERE table_line IS INITIAL.
  ENDMETHOD.
  
ENDCLASS.
