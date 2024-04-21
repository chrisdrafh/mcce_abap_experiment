CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
    METHODS count_characters
      IMPORTING iv_char  TYPE c
      CHANGING  ct_lines TYPE board_type
      RETURNING VALUE(rv_count) TYPE i.

    METHODS check_win
      IMPORTING iv_player TYPE player_type
      CHANGING  ct_board  TYPE board_type
      RETURNING VALUE(rv_win) TYPE abap_bool.

ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: lv_x_count TYPE i,
          lv_o_count TYPE i.

    lv_x_count = count_characters( player_enum-one, board ).
    lv_o_count = count_characters( player_enum-two, board ).

    IF lv_x_count < lv_o_count OR lv_x_count > lv_o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( player_enum-one, board ) AND check_win( player_enum-two, board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( player_enum-one, board ) OR check_win( player_enum-two, board ).
      state = state_enum-win.
    ELSEIF lv_x_count + lv_o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    DATA lv_count TYPE i.
    LOOP AT ct_lines INTO DATA(lv_line).
      lv_count += strlen( REPLACE ALL OCCURRENCES OF ' ' IN lv_line WITH '' ).
      lv_count -= strlen( REPLACE ALL OCCURRENCES OF iv_char IN lv_line WITH '' ).
    ENDLOOP.
    rv_count = lv_count.
  ENDMETHOD.

  METHOD check_win.
    rv_win = abap_false.

    " Horizontal, Vertical, and Diagonal checks
    LOOP AT ct_board INTO DATA(lv_line) FROM 1 TO 3.
      IF lv_line+0(1) = iv_player AND lv_line+1(1) = iv_player AND lv_line+2(1) = iv_player.
        rv_win = abap_true.
      ENDIF.
    ENDLOOP.

    " Vertical checks
    DO 3 TIMES.
      IF ct_board[ 1 ]+sy-index(1) = iv_player AND
         ct_board[ 2 ]+sy-index(1) = iv_player AND
         ct_board[ 3 ]+sy-index(1) = iv_player.
        rv_win = abap_true.
      ENDIF.
    ENDDO.

    " Diagonal checks
    IF ( ct_board[ 1 ]+0(1) = iv_player AND ct_board[ 2 ]+1(1) = iv_player AND ct_board[ 3 ]+2(1) = iv_player ) OR
       ( ct_board[ 1 ]+2(1) = iv_player AND ct_board[ 2 ]+1(1) = iv_player AND ct_board[ 3 ]+0(1) = iv_player ).
      rv_win = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
