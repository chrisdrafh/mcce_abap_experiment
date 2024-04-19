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
      IMPORTING board        TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.
    METHODS count_marks
      IMPORTING board         TYPE board_type
      RETURNING VALUE(count_x) TYPE i
                VALUE(count_o) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA(count_x) = count_marks( board = board ).
    DATA(count_o) = count_marks( board = board ).

    " Validate board
    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for winner
    DATA(winner) = check_winner( board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
      RETURN.
    ENDIF.

    " Check for draw or ongoing game
    IF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    " Check rows, columns and diagonals
    LOOP AT board INTO DATA(row).
      IF row CP 'XXX'.
        winner = 'X'.
        RETURN.
      ELSEIF row CP 'OOO'.
        winner = 'O'.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Add column and diagonal check logic here...

  ENDMETHOD.

  METHOD count_marks.
    count_x = 0.
    count_o = 0.
    LOOP AT board INTO DATA(line).
      count_x = count_x + strlen( replace( val = line sub = ' ' with = '' ) ).
      count_o = count_o + strlen( replace( val = line sub = ' ' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
