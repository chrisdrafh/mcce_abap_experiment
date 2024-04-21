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
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS get_state
      IMPORTING board TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_characters
      IMPORTING iv_char  TYPE c
                it_board TYPE board_type
      RETURNING VALUE(rv_count) TYPE i.

    METHODS check_win_condition
      IMPORTING iv_player TYPE player_type
                it_board  TYPE board_type
      RETURNING VALUE(rv_win) TYPE abap_bool.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: lv_x_count TYPE i,
          lv_o_count TYPE i.

    lv_x_count = count_characters( iv_char = player_enum-one, it_board = board ).
    lv_o_count = count_characters( iv_char = player_enum-two, it_board = board ).

    IF lv_x_count < lv_o_count OR lv_x_count > lv_o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(lv_x_wins) = check_win_condition( iv_player = player_enum-one, it_board = board ).
    DATA(lv_o_wins) = check_win_condition( iv_player = player_enum-two, it_board = board ).

    IF lv_x_wins AND lv_o_wins.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF lv_x_wins.
      state = state_enum-win.
    ELSEIF lv_o_wins.
      state = state_enum-win.
    ELSEIF lv_x_count + lv_o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    LOOP AT it_board INTO DATA(lv_row).
      rv_count += strlen( lv_row ) - strlen( replace( lv_row, iv_char, '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win_condition.
    LOOP AT it_board INTO DATA(lv_row).
      IF lv_row CS iv_player && lv_row CS iv_player && lv_row CS iv_player.
        rv_win = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: lv_diag1 TYPE player_type,
          lv_diag2 TYPE player_type.

    DO 3 TIMES.
      lv_diag1 = lv_diag1 && it_board[ sy-index ][ sy-index ].
      lv_diag2 = lv_diag2 && it_board[ sy-index ][ 4 - sy-index ].
    ENDDO.

    IF lv_diag1 CS iv_player && lv_diag1 CS iv_player && lv_diag1 CS iv_player OR
       lv_diag2 CS iv_player && lv_diag2 CS iv_player && lv_diag2 CS iv_player.
      rv_win = abap_true.
      RETURN.
    ENDIF.

    DO 3 TIMES.
      DATA(lv_col) TYPE player_type.
      lv_col = it_board[ 1 ][ sy-index ] && it_board[ 2 ][ sy-index ] && it_board[ 3 ][ sy-index ].
      IF lv_col CS iv_player && lv_col CS iv_player && lv_col CS iv_player.
        rv_win = abap_true.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
