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
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS count_marks
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(result) TYPE i.

    METHODS check_win
      IMPORTING
        board TYPE board_type
        player TYPE player_type
      RETURNING
        VALUE(is_win) TYPE abap_bool.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board = board
                           mark = player_enum-one ).
    o_count = count_marks( board = board
                           mark = player_enum-two ).

    " Check for invalid board scenarios
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for winning condition for both players
    IF check_win( board = board player = player_enum-one ) AND
       check_win( board = board player = player_enum-two ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( board = board player = player_enum-one ).
      state = state_enum-win.
    ELSEIF check_win( board = board player = player_enum-two ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    result = REDUCE i( INIT x = 0 FOR row IN board FOR mark IN row WHERE ( mark = player ) NEXT x = x + 1 ).
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns, and diagonals for winning conditions
    is_win = abap_false.

    " Checking rows and columns
    LOOP AT board INTO DATA(row) FROM 1 TO 3.
      IF row = player && player && player.
        is_win = abap_true.
      ENDIF.
    ENDLOOP.

    " Checking diagonals
    IF board[ 1 ][ 1 ] = player AND board[ 2 ][ 2 ] = player AND board[ 3 ][ 3 ] = player OR
       board[ 1 ][ 3 ] = player AND board[ 2 ][ 2 ] = player AND board[ 3 ][ 1 ] = player.
      is_win = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
