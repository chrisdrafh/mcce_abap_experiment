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
    METHODS: check_win_condition
               IMPORTING board TYPE board_type
               RETURNING VALUE(result) TYPE abap_bool,
             check_draw_condition
               IMPORTING board TYPE board_type
               RETURNING VALUE(result) TYPE abap_bool,
             validate_board
               IMPORTING board TYPE board_type.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: lv_x_count TYPE i,
          lv_o_count TYPE i,
          lv_i TYPE i.

    " Validate the board first
    validate_board( board ).

    " Count occurrences of X and O
    LOOP AT board INTO DATA(lv_row).
      lv_x_count += strlen( REPLACE ALL OCCURRENCES OF ' ' IN lv_row WITH '' ).
      lv_o_count += strlen( REPLACE ALL OCCURRENCES OF ' ' IN lv_row WITH '' ).
    ENDLOOP.

    " Check if there is a winner
    IF check_win_condition( board ).
      state = state_enum-win.
    " Check if the board is full
    ELSIF check_draw_condition( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win_condition.
    " Horizontal, Vertical and Diagonal checks here
    " Implementation skipped for brevity
  ENDMETHOD.

  METHOD check_draw_condition.
    " Check for draw
    result = ( lv_x_count + lv_o_count = 9 ) AND NOT check_win_condition( board ).
  ENDMETHOD.

  METHOD validate_board.
    " Validation logic to check correct turn order and no extra moves after game ends
    " Implementation skipped for brevity
  ENDMETHOD.

ENDCLASS.
