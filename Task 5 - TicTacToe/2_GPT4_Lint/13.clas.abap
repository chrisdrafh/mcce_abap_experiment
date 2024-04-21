CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3.

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
    METHODS check_win FOR TESTING
      IMPORTING line TYPE string
      RETURNING VALUE(result) TYPE boolean.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i,
          free_spaces TYPE i.

    " Count occurrences and free spaces
    LOOP AT board INTO DATA(row).
      count_x = count_x + strlen( replace( val = row regex = '[^X]' with = '' ) ).
      count_o = count_o + strlen( replace( val = row regex = '[^O]' with = '' ) ).
      free_spaces = free_spaces + strlen( replace( val = row regex = '[XO]' with = '' ) ).
    ENDLOOP.

    " Check for invalid board states
    IF abs( count_x - count_o ) > 1 OR ( count_o > count_x ).
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid turn order or excessive plays.'.
    ENDIF.

    " Check for wins and invalid continuation of play
    DATA(winner) = space.
    LOOP AT board INTO row.
      IF check_win( row ).
        IF winner IS INITIAL.
          winner = row+0(1).
        ELSEIF winner <> row+0(1).
          RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Multiple winners detected.'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA: transposed_board TYPE board_type.
    " Transpose the board to check columns
    DO 3 TIMES.
      transposed_board = VALUE #( BASE transposed_board
                                  ( FOR j = 1 UNTIL j > 3
                                    LET col_val = |{ board[ 1 ][ j ] }{ board[ 2 ][ j ] }{ board[ 3 ][ j ] }|
                                    IN col_val ) ).
    ENDDO.

    LOOP AT transposed_board INTO DATA(col).
      IF check_win( col ).
        IF winner IS INITIAL.
          winner = col+0(1).
        ELSEIF winner <> col+0(1).
          RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Multiple winners detected.'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Check diagonals
    DATA(diagonal1) = |{ board[ 1 ][ 1 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 3 ] }|.
    DATA(diagonal2) = |{ board[ 1 ][ 3 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 1 ] }|.
    IF check_win( diagonal1 ) OR check_win( diagonal2 ).
      IF winner IS INITIAL.
        winner = board[ 2 ][ 2 ].
      ELSEIF winner <> board[ 2 ][ 2 ].
        RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Multiple winners detected.'.
      ENDIF.
    ENDIF.

    " Determine final game state
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF free_spaces = 0.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    result = ( strlen( replace( val = line regex = '[^' && line+0(1) && ']' with = '' ) ) = 3 ).
  ENDMETHOD.

ENDCLASS.
