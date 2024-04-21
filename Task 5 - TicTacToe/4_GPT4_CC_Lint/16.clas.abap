CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

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

    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS count_marks
      IMPORTING
        board TYPE board_type
        mark  TYPE player_type
      RETURNING
        VALUE(count) TYPE i.

    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS is_full
      IMPORTING board TYPE board_type
      RETURNING VALUE(full) TYPE abap_bool.

ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type.

    validate_board( board ).

    winner = check_winner( board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF is_full( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board, player_enum-one ).
    o_count = count_marks( board, player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_winner( board ) IS NOT INITIAL AND x_count + o_count < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA: line TYPE string.

    LOOP AT board INTO line.
      count += strlen( CONDENSE( line ) ) - strlen( REPLACE( line, mark, '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    DATA: i TYPE i,
          diag1 TYPE string,
          diag2 TYPE string.

    LOOP AT board INTO DATA(row) INDEX INTO i.
      " Check rows
      IF row CS player_enum-one OR row CS player_enum-two.
        winner = row+0(1).
        EXIT.
      ENDIF.

      " Build diagonals
      diag1 = diag1 && row+i(1).
      diag2 = diag2 && row+2-i(1).
    ENDLOOP.

    " Check columns and diagonals
    IF winner IS INITIAL.
      LOOP AT board INTO row INDEX INTO i.
        IF board[ 1 ]+i(1) = board[ 2 ]+i(1) AND board[ 2 ]+i(1) = board[ 3 ]+i(1).
          winner = board[ 1 ]+i(1).
          EXIT.
        ENDIF.
      ENDLOOP.
      IF diag1 CO player_enum-one OR diag2 CO player_enum-two.
        winner = diag1+0(1).
      ENDIF.
      IF diag1 CO player_enum-two OR diag2 CO player_enum-one.
        winner = diag2+0(1).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_full.
    DATA(line) = CONDENSE( |{ board[ 1 ] }{ board[ 2 ] }{ board[ 3 ] }| ).
    full = xsdbool( strlen( line ) = 9 ).
  ENDMETHOD.

ENDCLASS.
