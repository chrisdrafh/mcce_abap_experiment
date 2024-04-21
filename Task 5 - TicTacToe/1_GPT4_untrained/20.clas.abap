CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH HEADER LINE.

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
    METHODS count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
                VALUE(o_count) TYPE i.
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.
    METHODS is_valid_board
      IMPORTING board TYPE board_type
      RETURNING VALUE(is_valid) TYPE abap_bool.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type,
          valid   TYPE abap_bool.

    x_count = count_marks( board = board ).
    o_count = count_marks( board = board ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order'.
    ENDIF.

    valid = is_valid_board( board = board ).
    IF valid = abap_false.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Board state invalid'.
    ENDIF.

    winner = check_winner( board = board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA: line TYPE string.
    x_count = 0.
    o_count = 0.
    LOOP AT board INTO line.
      x_count += strlen( replace( val = line regex = '[^X]' with = '' ) ).
      o_count += strlen( replace( val = line regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    " Logic to determine if there's a winner
  ENDMETHOD.

  METHOD is_valid_board.
    " Logic to check if the board is valid
    is_valid = abap_true.
  ENDMETHOD.

ENDCLASS.
