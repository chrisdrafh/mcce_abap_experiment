CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY INITIAL SIZE 3.

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
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_win_condition
      IMPORTING board  TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_player_moves
      IMPORTING board   TYPE board_type
                player  TYPE player_type
      RETURNING VALUE(count) TYPE i.

    CLASS-DATA: win_patterns TYPE TABLE OF board_type.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type.

    " Validate the board
    validate_board( board ).

    " Determine if there's a winner
    winner = check_win_condition( board ).

    " Determine game state based on the winner and board fullness
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF lines( board ) = 3 AND count_player_moves( board, 'X' ) + count_player_moves( board, 'O' ) = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_player_moves( board, 'X' ).
    DATA(o_count) = count_player_moves( board, 'O' ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win_condition.
    DATA: line TYPE string.
    LOOP AT board INTO line.
      IF line = 'XXX'.
        winner = 'X'.
        RETURN.
      ELSEIF line = 'OOO'.
        winner = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check verticals and diagonals...
  ENDMETHOD.

  METHOD count_player_moves.
    DATA: char TYPE c.
    count = 0.
    LOOP AT board INTO line.
      DO strlen( line ) TIMES.
        char = line+sy-index(1).
        IF char = player.
          count = count + 1.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
