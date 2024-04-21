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

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type OPTIONAL.
    
    METHODS count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.

    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner TYPE player_type OPTIONAL.

    " Validate board first
    validate_board( board ).

    " Count marks on the board
    count_marks( IMPORTING board    = board
                 EXPORTING x_count  = x_count
                           o_count  = o_count ).

    " Check if there's a winner
    winner = check_winner( board ).

    " Determine game state
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD validate_board.
    DATA: x_count TYPE i,
          o_count TYPE i.

    count_marks( IMPORTING board    = board
                 EXPORTING x_count  = x_count
                           o_count  = o_count ).

    IF o_count > x_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid number of Xs and Os'.
    ENDIF.

    " Check if someone has won and the game continued
    IF check_winner( board ) IS NOT INITIAL AND x_count + o_count < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Game continued after a win'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    DATA: lines TYPE TABLE OF string.

    " Define all possible winning lines
    lines = VALUE #( ( CONDENSE( board[ 1 ] ) )
                     ( CONDENSE( board[ 2 ] ) )
                     ( CONDENSE( board[ 3 ] ) )
                     ( CONDENSE( board[ 1 ][ 1 ] & board[ 2 ][ 1 ] & board[ 3 ][ 1 ] ) )
                     ( CONDENSE( board[ 1 ][ 2 ] & board[ 2 ][ 2 ] & board[ 3 ][ 2 ] ) )
                     ( CONDENSE( board[ 1 ][ 3 ] & board[ 2 ][ 3 ] & board[ 3 ][ 3 ] ) )
                     ( CONDENSE( board[ 1 ][ 1 ] & board[ 2 ][ 2 ] & board[ 3 ][ 3 ] ) )
                     ( CONDENSE( board[ 1 ][ 3 ] & board[ 2 ][ 2 ] & board[ 3 ][ 1 ] ) ) ).

    " Check if any line has three identical marks
    LOOP AT lines INTO DATA(line).
      IF line = 'XXX'.
        winner = 'X'.
        RETURN.
      ELSEIF line = 'OOO'.
        winner = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_marks.
    DATA: mark TYPE c LENGTH 1.

    x_count = 0.
    o_count = 0.

    LOOP AT board INTO DATA(row).
      DO 3 TIMES.
        mark = row+sy-index(1).
        IF mark = 'X'.
          x_count = x_count + 1.
        ELSEIF mark = 'O'.
          o_count = o_count + 1.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
