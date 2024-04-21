CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE player_type.
    METHODS count_chars
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type.

    count_chars( IMPORTING board    = board
                 RETURNING x_count  = x_count
                           o_count  = o_count ).

    " Check for invalid board state
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      " If a winner has been determined
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      " If the board is full and no winner
      state = state_enum-draw.
    ELSE.
      " Game is still ongoing
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    " Check rows, columns, and diagonals for a winner
    LOOP AT board INTO DATA(row) FROM 1.
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) IS NOT INITIAL.
        result = row(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns
    DO 3 TIMES.
      IF board[1](sy-index) = board[2](sy-index) AND board[2](sy-index) = board[3](sy-index) AND board[1](sy-index) IS NOT INITIAL.
        result = board[1](sy-index).
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF board  = board  AND board  = board  AND board  IS NOT INITIAL.
      result = board .
    ELSEIF board  = board  AND board  = board  AND board  IS NOT INITIAL.
      result = board .
    ENDIF.
  ENDMETHOD.

  METHOD count_chars.
    x_count = 0.
    o_count = 0.

    LOOP AT board INTO DATA(line).
      x_count = x_count + strlen( regex_replace( val = line regex = '[^X]' with = '' ) ).
      o_count = o_count + strlen( regex_replace( val = line regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
