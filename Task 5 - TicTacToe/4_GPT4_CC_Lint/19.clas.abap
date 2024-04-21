CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    "! E.g., ( ( `XOO` ) ( ` X ` ) ( `  X` ) )
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

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_marks RETURNING VALUE(result) TYPE i.
    METHODS check_winner RETURNING VALUE(result) TYPE boolean.
    METHODS validate_board RETURNING VALUE(result) TYPE boolean.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board first
    IF NOT validate_board( ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check if there's a winner
    DATA(winner) = check_winner( ).
    IF winner.
      state = state_enum-win.
    ELSE.
      " Check if the board is fully occupied or game is still ongoing
      DATA(marks) = count_marks( ).
      IF marks = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD validate_board.
    " Implementation details to verify if the board is valid, e.g., correct turn order, etc.
    " Assuming placeholder functionality here:
    result = abap_true.
  ENDMETHOD.

  METHOD count_marks.
    " Count the total number of marks on the board
    result = 0.
    LOOP AT board INTO DATA(line).
      result = result + strlen( replace( val = line regex = `[ ]` with = `` ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    " Check all rows, columns, and diagonals for a winning condition
    " Assuming placeholder functionality here:
    result = abap_false.
  ENDMETHOD.

ENDCLASS.
