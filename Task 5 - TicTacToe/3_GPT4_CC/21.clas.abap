CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_validity
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS has_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_characters
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    check_validity( board ).
    x_count, o_count = count_characters( board ).

    IF has_winner( board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_validity.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count, o_count = count_characters( board ).

    IF o_count > x_count OR x_count - o_count > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order or number of plays'.
    ENDIF.
  ENDMETHOD.

  METHOD has_winner.
    DATA: line TYPE string.

    LOOP AT board INTO DATA(row).
      " Check rows
      IF row CS 'XXX' OR row CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      line = board[ 1 ][ sy-index ] & board[ 2 ][ sy-index ] & board[ 3 ][ sy-index ].
      IF line = 'XXX' OR line = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonals
    line = board[ 1 ][ 1 ] & board[ 2 ][ 2 ] & board[ 3 ][ 3 ].
    IF line = 'XXX' OR line = 'OOO'.
      result = abap_true.
      RETURN.
    ENDIF.
    line = board[ 1 ][ 3 ] & board[ 2 ][ 2 ] & board[ 3 ][ 1 ].
    IF line = 'XXX' OR line = 'OOO'.
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
