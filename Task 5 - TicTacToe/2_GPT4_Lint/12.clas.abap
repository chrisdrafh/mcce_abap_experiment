CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3 WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    CLASS-METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS: count_player
      IMPORTING board       TYPE board_type
                player      TYPE player_type
      RETURNING VALUE(count) TYPE i.

    CLASS-METHODS: check_win
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS: check_full
      IMPORTING board  TYPE board_type
      RETURNING VALUE(full) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          game_won TYPE abap_bool,
          board_full TYPE abap_bool.

    x_count = count_player( board = board player = player_enum-one ).
    o_count = count_player( board = board player = player_enum-two ).

    " Check for invalid board conditions
    IF x_count - o_count NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check if any player has won
    LOOP AT player_enum INTO DATA(player).
      game_won = check_win( board = board player = player ).
      IF game_won = abap_true.
        state = state_enum-win.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check if the board is full
    board_full = check_full( board = board ).
    IF board_full = abap_true AND state IS INITIAL.
      state = state_enum-draw.
    ELSEIF state IS INITIAL.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_player.
    DATA(player_count) = 0.
    LOOP AT board INTO DATA(row).
      player_count += strlen( replace( val = row sub = |{ player }| with = | | occ = 0 ) ).
    ENDLOOP.
    count = player_count.
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns and diagonals for win condition
    DATA(win_conditions) = VALUE string_table( FOR i = 1 UNTIL i > 3
                      ( COND #( WHEN i = 1 THEN board[ i ]
                                WHEN i = 2 THEN |{ board[ 1 ]+1(1) }{ board[ 2 ]+1(1) }{ board[ 3 ]+1(1) }|
                                ELSE |{ board[ 1 ]+3(1) }{ board[ 2 ]+3(1) }{ board[ 3 ]+3(1) }| ) ) ).

    " Include diagonal checks
    APPEND |{ board[ 1 ]+1(1) }{ board[ 2 ]+2(1) }{ board[ 3 ]+3(1) }| TO win_conditions.
    APPEND |{ board[ 1 ]+3(1) }{ board[ 2 ]+2(1) }{ board[ 3 ]+1(1) }| TO win_conditions.

    LOOP AT win_conditions INTO DATA(condition).
      IF condition = repeat( val = player times = 3 ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    result = abap_false.
  ENDMETHOD.

  METHOD check_full.
    full = xsdbool( lines( replace( val = |{ board[ 1 ] }{ board[ 2 ] }{ board[ 3 ] }| with = | | occ = 0 ) ) = 9 ).
  ENDMETHOD.

ENDCLASS.
