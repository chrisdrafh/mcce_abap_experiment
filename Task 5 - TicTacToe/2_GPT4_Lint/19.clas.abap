CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: lv_x_count TYPE i,
          lv_o_count TYPE i,
          lv_winner  TYPE player_type.

    " Count X's and O's
    LOOP AT board INTO DATA(lv_row).
      lv_x_count += strlen( replace( val = lv_row regex = '[^X]' with = '' ) ).
      lv_o_count += strlen( replace( val = lv_row regex = '[^O]' with = '' ) ).
    ENDLOOP.

    " Check for invalid board: Wrong turn order or overplaying
    IF lv_x_count < lv_o_count OR lv_x_count > lv_o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid board: Incorrect number of X and O'.
    ENDIF.

    " Check for winner and if game was played after a win
    lv_winner = get_winner( board ).
    IF lv_winner IS NOT INITIAL AND lv_x_count + lv_o_count != 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid board: Game played after a win'.
    ENDIF.

    " Determine game state based on winner and board status
    IF lv_winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF lv_x_count + lv_o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD get_winner.
    " Check rows, columns, and diagonals for three in a line
    LOOP AT board INTO DATA(lv_row) FROM 1 TO 3.
      " Check rows
      IF lv_row CS 'XXX'.
        rv_winner = player_enum-one.
      ELSEIF lv_row CS 'OOO'.
        rv_winner = player_enum-two.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DATA: lv_diag1 TYPE string,
          lv_diag2 TYPE string.

    DO 3 TIMES.
      " Columns
      CONCATENATE lv_diag1 board[ 1 ][ sy-index ] INTO lv_diag1.
      CONCATENATE lv_diag2 board[ 2 ][ sy-index ] INTO lv_diag2.
      " Diagonals
      CONCATENATE lv_diag1 board[ sy-index ][ sy-index ] INTO lv_diag1.
      CONCATENATE lv_diag2 board[ sy-index ][ 4 - sy-index ] INTO lv_diag2.
    ENDDO.

    IF lv_diag1 CS 'XXX' OR lv_diag2 CS 'XXX'.
      rv_winner = player_enum-one.
    ELSEIF lv_diag1 CS 'OOO' OR lv_diag2 CS 'OOO'.
      rv_winner = player_enum-two.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
