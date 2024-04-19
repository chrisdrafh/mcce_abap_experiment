CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF player_enum,
             x TYPE c LENGTH 1 VALUE 'X',
             o TYPE c LENGTH 1 VALUE 'O',
           END OF player_enum,
           player_type TYPE player_enum-x.

    TYPES: board_type TYPE TABLE OF string INITIAL SIZE 3.

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
    METHODS check_if_valid_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS check_for_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type
      RAISING   cx_parameter_invalid.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board
    check_if_valid_board( board ).

    " Check if there's a winner
    DATA(winner) = check_for_winner( board ).

    " Determine the state of the game
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF lines( board ) = 3 AND lines( board[ 1 ] ) = 3 AND lines( board[ 2 ] ) = 3 AND lines( board[ 3 ] ) = 3.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_if_valid_board.
    DATA(x_count) = count( val = CONV string( board ) sub = `X` ).
    DATA(o_count) = count( val = CONV string( board ) sub = `O` ).

    " Check if O's and X's are in a valid order
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check if game continued after a win
    IF check_for_winner( board ) IS NOT INITIAL AND x_count + o_count = 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_for_winner.
    " Horizontal, Vertical, and Diagonal checks
    LOOP AT board INTO DATA(row) FROM 1.
      IF row = `XXX` OR row = `OOO`.
        winner = row+0(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Add more checks for verticals and diagonals
    " Implement similar checks for columns and diagonals
  ENDMETHOD.

ENDCLASS.
