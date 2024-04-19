CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE boolean.

    METHODS count_player
      IMPORTING board       TYPE board_type
                player      TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    validate_board( board ).
    DATA: count_x TYPE i,
          count_o TYPE i,
          is_win  TYPE boolean.

    count_x = count_player( board, player_enum-one ).
    count_o = count_player( board, player_enum-two ).
    is_win = check_win( board ).

    IF is_win.
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: count_x TYPE i,
          count_o TYPE i.

    count_x = count_player( board, player_enum-one ).
    count_o = count_player( board, player_enum-two ).

    IF abs( count_x - count_o ) > 1 OR count_x < count_o.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for multiple wins or playing after a win which is not handled in check_win.
    IF check_win( board ) AND count_x + count_o < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    DATA: line TYPE string.
    LOOP AT board INTO line.
      IF line = 'XXX' OR line = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      IF board[ 1 ][ sy-index ] = board[ 2 ][ sy-index ] AND board[ 2 ][ sy-index ] = board[ 3 ][ sy-index ] AND board[ 1 ][ sy-index ] IS NOT INITIAL.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF ( board[ 1 ][ 1 ] = board[ 2 ][ 2 ] AND board[ 2 ][ 2 ] = board[ 3 ][ 3 ] AND board[ 1 ][ 1 ] IS NOT INITIAL ) OR
       ( board[ 1 ][ 3 ] = board[ 2 ][ 2 ] AND board[ 2 ][ 2 ] = board[ 3 ][ 1 ] AND board[ 1 ][ 3 ] IS NOT INITIAL ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_player.
    DATA: line TYPE string.
    LOOP AT board INTO line.
      count += strlen( line ) - strlen( replace( line, player, '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
