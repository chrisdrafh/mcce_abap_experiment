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
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-DATA: invalid_board TYPE abap_bool.

    METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: check_board_validity
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid,
             count_occurrences
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(count) TYPE i,
             check_win_condition
      IMPORTING board TYPE board_type
      RETURNING VALUE(win) TYPE abap_bool,
             count_empty_cells
      IMPORTING board TYPE board_type
      RETURNING VALUE(empty_cells) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          win     TYPE abap_bool.

    x_count = count_occurrences( board, player_enum-one ).
    o_count = count_occurrences( board, player_enum-two ).

    " Check board validity based on player count and win conditions
    check_board_validity( board ).

    " Determine if there's a winner
    win = check_win_condition( board ).

    IF win = abap_true.
      state = state_enum-win.
    ELSEIF count_empty_cells( board ) = 0.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_board_validity.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_occurrences( board, player_enum-one ).
    o_count = count_occurrences( board, player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD count_occurrences.
    DATA: cell TYPE string.
    count = 0.
    LOOP AT board INTO cell.
      count += strlen( replace_all( val = cell sub = space with = `` ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win_condition.
    win = abap_false.
    " Implement win checking logic (rows, columns, diagonals)
  ENDMETHOD.

  METHOD count_empty_cells.
    DATA: cell TYPE string.
    empty_cells = 0.
    LOOP AT board INTO cell.
      empty_cells += 3 - strlen( replace_all( val = cell sub = space with = `` ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
