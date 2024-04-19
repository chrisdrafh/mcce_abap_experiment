CLASS zcl_word_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF return_structure,
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
    METHODS split_into_words
      IMPORTING
        !phrase TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: lt_words TYPE TABLE OF string,
          ls_result TYPE return_structure.

    lt_words = split_into_words( phrase ).

    LOOP AT lt_words INTO DATA(lv_word).
      READ TABLE result WITH KEY word = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_result-word = lv_word.
        ls_result-count = 1.
        INSERT ls_result INTO TABLE result.
      ELSE.
        READ TABLE result WITH KEY word = lv_word INTO ls_result.
        ls_result-count += 1.
        MODIFY result FROM ls_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_into_words.
    " Split the phrase into words based on punctuation and whitespace
    " Remove all unwanted punctuation using regular expressions
    " Replace them with space to split easily
    DATA(lv_phrase) = to_lower( phrase ). " Convert to lowercase for case insensitivity
    REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9'']' IN lv_phrase WITH ' '.
    SPLIT lv_phrase AT ' ' INTO TABLE words.
    DELETE words WHERE table_line IS INITIAL.
  ENDMETHOD.

ENDCLASS.
