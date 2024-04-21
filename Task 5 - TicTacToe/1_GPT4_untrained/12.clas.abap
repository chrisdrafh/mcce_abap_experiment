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
      IMPORTING board TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_valid_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(has_winner) TYPE abap_bool.

    METHODS count_symbols
      IMPORTING board      TYPE board_type
                symbol     TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    check_valid_board( board ).

    DATA(has_winner) = check_winner( board ).
    IF has_winner = abap_true.
      state = state_enum-win.
    ELSE.
      DATA(count_x) = count_symbols( board, player_enum-one ).
      DATA(count_o) = count_symbols( board, player_enum-two ).
      IF count_x + count_o = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_valid_board.
    DATA(count_x) = count_symbols( board, player_enum-one ).
    DATA(count_o) = count_symbols( board, player_enum-two ).
    IF count_x < count_o OR count_x - count_o > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = 'Invalid board: incorrect number of X and O'.
    ENDIF.

    IF check_winner( board ) AND count_x + count_o < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING textid = 'Invalid board: continued after win'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row).
      IF row = 'XXX' OR row = 'OOO'.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DATA(win_patterns) = VALUE stringtab( BASE 'XXX' 'OOO' ).
    DATA(diagonal1) = |{ board[1][1] }{ board[2][2] }{ board[3][3] }|.
    DATA(diagonal2) = |{ board[1][3] }{ board[2][2] }{ board[3][1] }|.
    LOOP AT win_patterns INTO DATA(pattern).
      " Check columns
      LOOP AT 1 TO 3 INTO DATA(col_idx).
        DATA(column) = |{ board[1][col_idx] }{ board[2][col_idx] }{ board[3][col_idx] }|.
        IF column = pattern.
          has_winner = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.

      " Check diagonals
      IF diagonal1 = pattern OR diagonal2 = pattern.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_symbols.
    LOOP AT board INTO DATA(row).
      count += strlen( regex_replace( val = row regex = |[^{ symbol }]| occ = 0 with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
