CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

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
      RETURNING VALUE(is_valid) TYPE abap_bool.

    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(has_winner) TYPE abap_bool.

    METHODS count_characters
      IMPORTING board      TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.
    DATA(is_valid) = validate_board( board ).

    IF NOT is_valid.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    x_count = count_characters( board = board )-x_count.
    o_count = count_characters( board = board )-o_count.

    IF check_winner( board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_characters( board = board )-x_count.
    DATA(o_count) = count_characters( board = board )-o_count.
    is_valid = x_count = o_count + 1 OR x_count = o_count.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row).
      IF row CP 'XXX' OR row CP 'OOO'.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA(vertical) TYPE board_type.
    vertical = VALUE #(
      ( CONDENSE( board[ 1 ][ 1 ] && board[ 2 ][ 1 ] && board[ 3 ][ 1 ] ) )
      ( CONDENSE( board[ 1 ][ 2 ] && board[ 2 ][ 2 ] && board[ 3 ][ 2 ] ) )
      ( CONDENSE( board[ 1 ][ 3 ] && board[ 2 ][ 3 ] && board[ 3 ][ 3 ] ) )
    ).

    LOOP AT vertical INTO row.
      IF row CP 'XXX' OR row CP 'OOO'.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA(diagonal1) = CONDENSE( board[ 1 ][ 1 ] && board[ 2 ][ 2 ] && board[ 3 ][ 3 ] ).
    DATA(diagonal2) = CONDENSE( board[ 1 ][ 3 ] && board[ 2 ][ 2 ] && board[ 3 ][ 1 ] ).

    IF diagonal1 CP 'XXX' OR diagonal1 CP 'OOO' OR diagonal2 CP 'XXX' OR diagonal2 CP 'OOO'.
      has_winner = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    LOOP AT board INTO DATA(row).
      x_count += strlen( REPLACE ALL OCCURRENCES OF 'O' IN row WITH '' ).
      o_count += strlen( REPLACE ALL OCCURRENCES OF 'X' IN row WITH '' ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
