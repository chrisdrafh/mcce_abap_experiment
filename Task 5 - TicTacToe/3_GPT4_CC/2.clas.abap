CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3.

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

  PRIVATE SECTION.
    METHODS count_characters
      IMPORTING iv_char TYPE c
                it_board TYPE board_type
      RETURNING VALUE(rv_count) TYPE i.

    METHODS check_winner
      IMPORTING iv_player TYPE player_type
                it_board  TYPE board_type
      RETURNING VALUE(rv_winner) TYPE abap_bool.

ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: lv_x_count TYPE i,
          lv_o_count TYPE i,
          lv_winner_found TYPE abap_bool.

    lv_x_count = count_characters( iv_char = 'X' it_board = board ).
    lv_o_count = count_characters( iv_char = 'O' it_board = board ).

    " Validate correct turn order and correct number of plays
    IF lv_x_count - lv_o_count NOT IN 0 AND 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = 'Invalid board: Turn order is incorrect'.
    ENDIF.

    " Check for winners
    IF check_winner( iv_player = 'X' it_board = board ).
      lv_winner_found = abap_true.
      IF lv_o_count >= lv_x_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid
          EXPORTING textid = 'Invalid board: X wins but O played after'.
      ENDIF.
    ENDIF.
    IF check_winner( iv_player = 'O' it_board = board ).
      IF lv_winner_found = abap_true.
        RAISE EXCEPTION TYPE cx_parameter_invalid
          EXPORTING textid = 'Invalid board: Both players cannot win'.
      ENDIF.
      lv_winner_found = abap_true.
      IF lv_x_count > lv_o_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid
          EXPORTING textid = 'Invalid board: O wins but X played after'.
      ENDIF.
    ENDIF.

    " Determine game state based on board analysis
    IF lv_winner_found = abap_true.
      state = state_enum-win.
    ELSEIF lv_x_count + lv_o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_characters.
    rv_count = 0.
    LOOP AT it_board INTO DATA(lv_row).
      rv_count = rv_count + strlen( replace( val = lv_row regex = '[^' && iv_char && ']' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    DATA: lt_patterns TYPE TABLE OF string.

    " Generate all win patterns
    lt_patterns = VALUE #( ( `XXX......` ) ( `...XXX...` ) ( `......XXX` )
                           ( `X..X..X..` ) ( `.X..X..X.` ) ( `..X..X..X` )
                           ( `X...X...X` ) ( `..X.X.X..` ) ).
    " Check each pattern
    LOOP AT lt_patterns INTO DATA(lv_pattern).
      IF lv_pattern CP CONDENSE( it_board[ 1 ] && it_board[ 2 ] && it_board[ 3 ] USING SPACE ).
        rv_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_winner = abap_false.
  ENDMETHOD.

ENDCLASS.
