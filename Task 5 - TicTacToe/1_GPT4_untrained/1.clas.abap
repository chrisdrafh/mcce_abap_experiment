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
    METHODS check_for_winner
      IMPORTING board        TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_marks
      IMPORTING board TYPE board_type
      EXPORTING x_count TYPE i
                o_count TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.
    DATA: winner TYPE player_type.
    DATA: possible_win TYPE abap_bool VALUE abap_false.

    count_marks(
      EXPORTING board = board
      IMPORTING x_count = x_count
                o_count = o_count ).

    " Validate counts
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for a winner
    winner = check_for_winner( board ).

    " Determine the game state based on the winner and counts
    IF winner IS NOT INITIAL.
      possible_win = abap_true.
    ENDIF.

    IF possible_win = abap_true.
      IF x_count <> o_count + 1 AND winner = player_enum-one.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ELSEIF o_count <> x_count AND winner = player_enum-two.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_for_winner.
    " Check all possible winning combinations
    DATA: idx TYPE i.
    DATA(win_patterns) = VALUE itab_t(
        ( CONV #( '123' ) )
        ( CONV #( '456' ) )
        ( CONV #( '789' ) )
        ( CONV #( '147' ) )
        ( CONV #( '258' ) )
        ( CONV #( '369' ) )
        ( CONV #( '159' ) )
        ( CONV #( '357' ) ) ).

    LOOP AT win_patterns INTO DATA(pattern).
      DATA(line) = board( CONV i( pattern(1) ) - 1 ) && 
                   board( CONV i( pattern(2) ) - 1 ) &&
                   board( CONV i( pattern(3) ) - 1 ).
      IF line = 'XXX'.
        winner = player_enum-one.
        RETURN.
      ELSEIF line = 'OOO'.
        winner = player_enum-two.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_marks.
    DATA: line TYPE string.
    LOOP AT board INTO line.
      x_count = x_count + strlen( CONDENSE( REPLACE( line WITH 'O' INTO '' ) ) ).
      o_count = o_count + strlen( CONDENSE( REPLACE( line WITH 'X' INTO '' ) ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
