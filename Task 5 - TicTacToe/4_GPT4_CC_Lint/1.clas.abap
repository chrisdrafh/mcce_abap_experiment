CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_chars
      IMPORTING board TYPE board_type
                char  TYPE c
      RETURNING VALUE(count) TYPE i.
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate board first
    validate_board( board ).

    " Check win condition
    IF check_win( board ).
      state = state_enum-win.
    ELSE.
      " Check if draw or ongoing
      DATA(x_count) = count_chars( board, player_enum-one ).
      DATA(o_count) = count_chars( board, player_enum-two ).

      IF x_count + o_count = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns, and diagonals for a win
    LOOP AT board INTO DATA(row).
      IF row CS 'XXX' OR row CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    LOOP AT board TRANSPORTING NO FIELDS.
      DATA(column_win) = abap_true.
      DATA(diagonal1) = |{ board[1][1] }{ board[2][2] }{ board[3][3] }|.
      DATA(diagonal2) = |{ board[1][3] }{ board[2][2] }{ board[3][1] }|.

      IF diagonal1 CS 'XXX' OR diagonal1 CS 'OOO' OR diagonal2 CS 'XXX' OR diagonal2 CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.

      " Check each column
      DO 3 TIMES.
        DATA(column) = |{ board[1][sy-index] }{ board[2][sy-index] }{ board[3][sy-index] }|.
        IF NOT column CS 'XXX' AND NOT column CS 'OOO'.
          column_win = abap_false.
          EXIT.
        ENDIF.
      ENDDO.

      IF column_win = abap_true.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_chars.
    LOOP AT board INTO DATA(row).
      count += strlen( replace( val = row regex = '[^' && char && ']' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_chars( board, player_enum-one ).
    DATA(o_count) = count_chars( board, player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
