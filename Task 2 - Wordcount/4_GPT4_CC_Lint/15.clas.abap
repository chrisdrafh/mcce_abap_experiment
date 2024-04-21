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
    METHODS normalize_text
      IMPORTING
        !phrase TYPE string
      RETURNING
        VALUE(normalized_phrase) TYPE string.

    METHODS split_phrase_into_words
      IMPORTING
        !phrase TYPE string
      RETURNING
        VALUE(words) TYPE TABLE OF string.
ENDCLASS.


CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: normalized_text TYPE string,
          words           TYPE TABLE OF string,
          word_map        TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line,
          word_count      TYPE i.

    normalized_text = normalize_text( phrase ).
    words = split_phrase_into_words( normalized_text ).

    LOOP AT words INTO DATA(word).
      word = to_lower( word ).
      READ TABLE word_map ASSIGNING FIELD-SYMBOL(<entry>) WITH KEY table_line = word.
      IF sy-subrc = 0.
        ADD 1 TO <entry>.
      ELSE.
        word_map[ word ] = 1.
      ENDIF.
    ENDLOOP.

    LOOP AT word_map INTO DATA(word_entry) WITH KEY table_line ASSIGNING <final_entry>.
      APPEND VALUE return_structure( word = word_entry-table_line, count = word_entry ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_text.
    " Normalize text to handle punctuation and ensure only valid word separators are used
    REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9'\s]' IN phrase WITH ' '.
    normalized_phrase = phrase.
  ENDMETHOD.

  METHOD split_phrase_into_words.
    " Split the normalized phrase into words using space as a delimiter
    SPLIT phrase AT space INTO TABLE words.
  ENDMETHOD.
ENDCLASS.
