CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

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
    METHODS check_win CONDITIONAL FOR player_type
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_characters
      IMPORTING board      TYPE board_type
                char       TYPE player_type
      RETURNING VALUE(num) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_characters( board, player_enum-one ).
    o_count = count_characters( board, player_enum-two ).

    " Check for invalid board situations
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for wins
    IF check_win( board ) = abap_true.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    " Check all rows, columns and diagonals
    LOOP AT board INTO DATA(row).
      IF row = player_enum-one && row+1 = player_enum-one && row+2 = player_enum-one.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Additional win checks for columns and diagonals needed

  ENDMETHOD.

  METHOD count_characters.
    LOOP AT board INTO DATA(row).
      num += strlen( replace( val = row regex = '[^' && char && ']' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
