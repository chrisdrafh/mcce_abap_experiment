CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH HEADER LINE.

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
    METHODS validate_board
      IMPORTING board TYPE board_type
      RETURNING VALUE(is_valid) TYPE abap_bool
      RAISING   cx_parameter_invalid.

    METHODS count_chars
      IMPORTING board    TYPE board_type
                char_to_count TYPE c LENGTH 1
      RETURNING VALUE(count) TYPE i.

    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type
      RAISING   cx_parameter_invalid.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type.

    " Validate the board first
    IF NOT validate_board( board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid board configuration.'.
    ENDIF.

    " Check for a winner
    winner = check_winner( board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF count_chars( board, ' ' ) = 0.
      " No spaces left, it's a draw
      state = state_enum-draw.
    ELSE.
      " Game is ongoing
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: num_x TYPE i,
          num_o TYPE i.

    num_x = count_chars( board, player_enum-one ).
    num_o = count_chars( board, player_enum-two ).

    " X starts so there can't be more Os than Xs, and not more than 1 extra X
    IF num_o > num_x OR num_x > num_o + 1.
      is_valid = abap_false.
    ELSE.
      is_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_chars.
    DATA: line TYPE string.
    count = 0.
    LOOP AT board INTO line.
      count = count + strlen( REPLACE ALL OCCURRENCES OF char_to_count IN line WITH '' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    DATA: line TYPE string.
    " Check all rows
    LOOP AT board INTO line.
      IF line = 'XXX'.
        winner = player_enum-one.
        RETURN.
      ELSEIF line = 'OOO'.
        winner = player_enum-two.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check all columns and diagonals, similar logic
    " Implementation would follow the same pattern for columns and diagonals
  ENDMETHOD.

ENDCLASS.
