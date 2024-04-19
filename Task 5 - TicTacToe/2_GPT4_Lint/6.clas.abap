CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          empty_count TYPE i.

    " Count occurrences of X, O, and empty spaces
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
      empty_count += strlen( regex_replace( val = row regex = '[^ ]' with = '' global = abap_true ) ).
    ENDLOOP.

    " Validate the board state
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid board: Incorrect turn order'.
    ENDIF.

    DATA(win_x) = check_winner( board = board player = player_enum-one ).
    DATA(win_o) = check_winner( board = board player = player_enum-two ).

    IF win_x = abap_true AND win_o = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid board: Multiple winners'.
    ENDIF.
    
    IF win_x = abap_true.
      state = state_enum-win.
    ELSIF win_o = abap_true.
      state = state_enum-win.
    ELSEIF empty_count = 0.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    DATA(result) = abap_false.
    DATA: lines TYPE TABLE OF string.

    " Check rows
    lines = board.

    " Check columns
    DO 3 TIMES.
      lines = VALUE #( BASE lines ( CONDENSE( board[ 1 ][ sy-index ] && board[ 2 ][ sy-index ] && board[ 3 ][ sy-index ] ) ) ).
    ENDDO.

    " Check diagonals
    lines = VALUE #( BASE lines ( CONDENSE( board[ 1 ][ 1 ] && board[ 2 ][ 2 ] && board[ 3 ][ 3 ] ) )
                            ( CONDENSE( board[ 1 ][ 3 ] && board[ 2 ][ 2 ] && board[ 3 ][ 1 ] ) ) ).

    " Check for winner
    LOOP AT lines INTO DATA(line).
      IF line = player && player && player.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    RETURN result.
  ENDMETHOD.

ENDCLASS.
