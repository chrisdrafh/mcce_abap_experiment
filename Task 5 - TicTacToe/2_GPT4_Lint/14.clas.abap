CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

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

  PRIVATE SECTION.
    METHODS count_chars
      IMPORTING iv_string TYPE string
                iv_char   TYPE c
      RETURNING VALUE(rv_count) TYPE i.

    METHODS check_win_condition
      IMPORTING iv_player TYPE player_type
                iv_board  TYPE board_type
      RETURNING VALUE(rv_won) TYPE abap_bool.

    METHODS is_board_valid
      IMPORTING iv_board TYPE board_type
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board first
    IF NOT is_board_valid( board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check win conditions
    IF check_win_condition( player_enum-one, board ).
      state = state_enum-win.
    ELSEIF check_win_condition( player_enum-two, board ).
      state = state_enum-win.
    ELSE.
      " Check for draw or ongoing game
      DATA(lv_count_x) = count_chars( iv_string = CONCATENATE LINES OF board, iv_char = 'X' ).
      DATA(lv_count_o) = count_chars( iv_string = CONCATENATE LINES OF board, iv_char = 'O' ).

      IF lv_count_x + lv_count_o = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD count_chars.
    rv_count = strlen( REPLACE ALL OCCURRENCES OF iv_char IN iv_string WITH '' ).
    rv_count = strlen( iv_string ) - rv_count.
  ENDMETHOD.

  METHOD check_win_condition.
    rv_won = abap_false.
    DATA: lt_rows TYPE board_type.
    lt_rows = iv_board.

    " Check rows and columns
    LOOP AT lt_rows INTO DATA(lv_row).
      IF lv_row = CONDENSE( iv_player && iv_player && iv_player ).
        rv_won = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check diagonals
    IF lt_rows  = iv_player AND lt_rows  = iv_player AND lt_rows  = iv_player.
      rv_won = abap_true.
      RETURN.
    ENDIF.

    IF lt_rows  = iv_player AND lt_rows  = iv_player AND lt_rows  = iv_player.
      rv_won = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD is_board_valid.
    " Logic to determine if the board state is valid
    DATA(lv_count_x) = count_chars( iv_string = CONCATENATE LINES OF iv_board, iv_char = 'X' ).
    DATA(lv_count_o) = count_chars( iv_string = CONCATENATE LINES OF iv_board, iv_char = 'O' ).

    " X starts, so there must be at most one more X than O
    rv_valid = lv_count_x - lv_count_o <= 1 AND lv_count_x - lv_count_o >= 0.

    " Further validation can be added based on game-specific rules
  ENDMETHOD.

ENDCLASS.
