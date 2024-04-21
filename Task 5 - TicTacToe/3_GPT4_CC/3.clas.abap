CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY INITIAL SIZE 3,
           t_counts TYPE i.

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
    METHODS count_players
      IMPORTING board         TYPE board_type
      RETURNING VALUE(counts) TYPE t_counts.

    METHODS check_winner
      IMPORTING board         TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS validate_board
      IMPORTING board         TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    IF NOT validate_board( board ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(winner) = check_winner( board ).
    DATA(x_count) = count_players( board ).

    IF winner = abap_true.
      state = state_enum-win.
    ELSEIF x_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_players( board ).
    DATA(o_count) = count_players( board ).

    IF x_count - o_count NOT IN 0 AND 1.
      result = abap_false.
    ELSE.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    " Check each row, column, and diagonals for three identical non-empty characters
    " Implementation goes here
    " This should set result to abap_true if a winner is found, else abap_false
  ENDMETHOD.

  METHOD count_players.
    " Count occurrences of X and O
    " Implementation goes here
    " Return a structured count of Xs and Os
  ENDMETHOD.

ENDCLASS.
