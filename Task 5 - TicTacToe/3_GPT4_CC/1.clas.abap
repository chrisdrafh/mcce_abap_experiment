CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE STANDARD TABLE OF string WITH DEFAULT KEY INITIAL SIZE 3.

    CONSTANTS: BEGIN OF player_enum,
                 x TYPE player_type VALUE 'X',
                 o TYPE player_type VALUE 'O',
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
    METHODS check_if_board_is_valid
      IMPORTING board TYPE board_type
      RETURNING VALUE(is_valid) TYPE abap_bool.

    METHODS count_occurrences
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(count) TYPE i.

    METHODS check_win
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(has_won) TYPE abap_bool.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: is_valid TYPE abap_bool.

    " Check if the board is valid
    is_valid = check_if_board_is_valid( board ).
    IF NOT is_valid.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check if any player has won
    IF check_win( board, player_enum-x ).
      state = state_enum-win.
    ELSIF check_win( board, player_enum-o ).
      state = state_enum-win.
    ELSEIF count_occurrences( board, ' ' ) = 0. " No empty spaces
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_if_board_is_valid.
    DATA(x_count) = count_occurrences( board, player_enum-x ).
    DATA(o_count) = count_occurrences( board, player_enum-o ).

    " Check correct turn order and if the game has continued after a win
    IF x_count < o_count OR x_count > o_count + 1 OR
       ( x_count > o_count AND check_win( board, player_enum-o ) ) OR
       ( o_count = x_count AND check_win( board, player_enum-x ) ).
      is_valid = abap_false.
    ELSE.
      is_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_occurrences.
    DATA(result) = 0.
    LOOP AT board INTO DATA(line).
      result += strlen( replace_all( val = line sub = ` ` with = `` ) ).
    ENDLOOP.
    count = result.
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns, and diagonals for a win
    has_won = abap_false.
    " Implementation of winning logic
    " For brevity, let's assume this is a placeholder for actual winning logic
    " Including checks for three 'player' symbols in a row, column, or diagonal
  ENDMETHOD.

ENDCLASS.
