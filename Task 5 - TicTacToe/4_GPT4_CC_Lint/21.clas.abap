CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3.

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
      IMPORTING board TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_marks
      IMPORTING board TYPE board_type
      EXPORTING x_count TYPE i
                o_count TYPE i.
    METHODS check_win
      IMPORTING board   TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.
    METHODS board_is_full
      IMPORTING board   TYPE board_type
      RETURNING VALUE(is_full) TYPE abap_bool.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type,
          is_full TYPE abap_bool.

    count_marks( EXPORTING board   = board
                 IMPORTING x_count = x_count
                           o_count = o_count ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order or extra moves detected'.
    ENDIF.

    winner  = check_win( board ).
    is_full = board_is_full( board ).

    IF winner IS NOT INITIAL AND winner = player_enum-one AND x_count = o_count + 1.
      state = state_enum-win.
    ELSEIF winner IS NOT INITIAL AND winner = player_enum-two AND x_count = o_count.
      state = state_enum-win.
    ELSEIF is_full = abap_true AND winner IS INITIAL.
      state = state_enum-draw.
    ELSEIF winner IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Game continued after a win'.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA(line) = VALUE string_table( ).

    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns and diagonals for a winner
    DATA: win_patterns TYPE string_table,
          diag1 TYPE string,
          diag2 TYPE string.

    win_patterns = VALUE #(
      ( board[ 1 ] ) ( board[ 2 ] ) ( board[ 3 ] )
      ( FOR i = 1 THEN i + 1 UNTIL i > 3 ( |{ board[ 1 ][ i ] }{ board[ 2 ][ i ] }{ board[ 3 ][ i ] }| ) )
    ).

    diag1 = |{ board[ 1 ][ 1 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 3 ] }|.
    diag2 = |{ board[ 1 ][ 3 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 1 ] }|.
    APPEND diag1 TO win_patterns.
    APPEND diag2 TO win_patterns.

    LOOP AT win_patterns INTO DATA(pattern).
      IF pattern = 'XXX'.
        winner = player_enum-one.
        EXIT.
      ELSEIF pattern = 'OOO'.
        winner = player_enum-two.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD board_is_full.
    is_full = boolc( regex_match( val = |{ board[ 1 ] }{ board[ 2 ] }{ board[ 3 ] }| regex = '^[XO]{9}$' ) ).
  ENDMETHOD.

ENDCLASS.
