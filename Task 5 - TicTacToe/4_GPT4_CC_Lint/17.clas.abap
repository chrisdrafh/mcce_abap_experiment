CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-DATA: win_conditions TYPE TABLE OF string
                                  VALUE `111000000;000111000;000000111;100100100;010010010;001001001;100010001;001010100`.

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
      RETURNING VALUE(is_win) TYPE abap_bool.

    METHODS count_player_moves
      IMPORTING board     TYPE board_type
                player    TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " First, validate the board to catch any invalid states
    validate_board( board ).

    " Check for win conditions
    IF check_win( board ).
      state = state_enum-win.
    ELSE.
      " Check if all cells are filled
      DATA(cells_filled) = 0.
      LOOP AT board INTO DATA(line).
        cells_filled = cells_filled + strlen( replace( val = line regex = '\s' with = '' ) ).
      ENDLOOP.

      " Determine if the game is ongoing or a draw
      IF cells_filled = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    " Validate if the board has a correct number of 'X' and 'O' and check for invalid games
    DATA(x_count) = count_player_moves( board = board player = player_enum-one ).
    DATA(o_count) = count_player_moves( board = board player = player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    " Check all win conditions
    LOOP AT win_conditions INTO DATA(condition).
      DATA(result) = 0.
      DO 9 TIMES.
        IF condition+sy-index(1) = '1'.
          IF board[ sy-index DIV 3 + 1 ]( sy-index MOD 3 + 1 ) = player_enum-one.
            result = result + 1.
          ENDIF.
        ENDIF.
      ENDDO.
      IF result = 3.
        is_win = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    is_win = abap_false.
  ENDMETHOD.

  METHOD count_player_moves.
    count = 0.
    LOOP AT board INTO DATA(line).
      count = count + strlen( replace( val = line regex = player with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
