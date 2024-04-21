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

    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS count_symbols
      IMPORTING
        board   TYPE board_type
        symbol  TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    validate_board( board ).

    IF check_win( board ).
      state = state_enum-win.
    ELSEIF count_symbols( board, 'X' ) + count_symbols( board, 'O' ) = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_symbols( board, player_enum-one ).
    DATA(o_count) = count_symbols( board, player_enum-two ).

    IF o_count > x_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( board ) AND x_count <> o_count + 1 AND o_count <> x_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<row>).
      IF <row> CS 'XXX' OR <row> CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    result = abap_false.
  ENDMETHOD.

  METHOD count_symbols.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      count += strlen( replace_all( val = <line> sub = ' ' with = '' ) ).
    ENDLOOP.
    count = count DIV 3.
  ENDMETHOD.

ENDCLASS.
