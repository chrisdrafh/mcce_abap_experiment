CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH EMPTY KEY.

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
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.
    METHODS count_chars
      IMPORTING board TYPE board_type
      EXPORTING x_count TYPE i
                o_count TYPE i.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    validate_board( board ).

    count_chars( IMPORTING board = board
                 EXPORTING x_count = x_count
                           o_count = o_count ).

    DATA(winner) = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count TYPE i).
    DATA(o_count TYPE i).

    count_chars( IMPORTING board = board
                 EXPORTING x_count = x_count
                           o_count = o_count ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(line).
      IF line CS 'XXX'.
        winner = 'X'.
        RETURN.
      ELSEIF line CS 'OOO'.
        winner = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA(transpose_board) = VALUE board_type(
      ( CONV string( board[ 1 ][ 1 ] & board[ 2 ][ 1 ] & board[ 3 ][ 1 ] ) )
      ( CONV string( board[ 1 ][ 2 ] & board[ 2 ][ 2 ] & board[ 3 ][ 2 ] ) )
      ( CONV string( board[ 1 ][ 3 ] & board[ 2 ][ 3 ] & board[ 3 ][ 3 ] ) )
    ).

    LOOP AT transpose_board INTO line.
      IF line CS 'XXX'.
        winner = 'X'.
        RETURN.
      ELSEIF line CS 'OOO'.
        winner = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA(diagonal1) = board[ 1 ][ 1 ] & board[ 2 ][ 2 ] & board[ 3 ][ 3 ].
    DATA(diagonal2) = board[ 1 ][ 3 ] & board[ 2 ][ 2 ] & board[ 3 ][ 1 ].

    IF diagonal1 = 'XXX' OR diagonal2 = 'XXX'.
      winner = 'X'.
      RETURN.
    ELSEIF diagonal1 = 'OOO' OR diagonal2 = 'OOO'.
      winner = 'O'.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_chars.
    x_count = 0.
    o_count = 0.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
