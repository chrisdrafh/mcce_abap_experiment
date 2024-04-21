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
        VALUE(result) TYPE return_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS normalize_text
      IMPORTING
        !text         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS split_into_words
      IMPORTING
        !text         TYPE string
      RETURNING
        VALUE(words)  TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_word_count IMPLEMENTATION.

  METHOD count_words.
    DATA: normalized_text TYPE string,
          words           TYPE TABLE OF string,
          word_occurrence TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.

    normalized_text = normalize_text( phrase ).
    words = split_into_words( normalized_text ).

    LOOP AT words INTO DATA(word).
      IF word_occurrence[ word ] IS INITIAL.
        word_occurrence[ word ] = 1.
      ELSE.
        word_occurrence[ word ] += 1.
      ENDIF.
    ENDLOOP.

    LOOP AT word_occurrence INTO DATA(word_count_pair) WITH KEY table_line COMPONENTS key = DATA(word_key) value = DATA(count).
    APPEND VALUE #( word = word_key count = count ) TO result.
  ENDLOOP.
ENDMETHOD.

METHOD normalize_text.
  result = to_lower( text ). " Normalize to lower case for case insensitivity
ENDMETHOD.

METHOD split_into_words.
    " Regular expression to split by spaces, punctuation except apostrophes
  SPLIT text AT '\s+|\b[^\w'']+|\b' INTO TABLE words.
ENDMETHOD.

ENDCLASS.
