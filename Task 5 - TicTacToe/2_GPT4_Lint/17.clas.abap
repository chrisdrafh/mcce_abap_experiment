CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
    METHODS check_win FOR BOARD
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE boolean.

    METHODS count_marks FOR BOARD
      IMPORTING board      TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          is_win  TYPE abap_bool.

    count_marks( EXPORTING board = board IMPORTING x_count = x_count o_count = o_count ).

    " Check if the board is invalid
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    is_win = check_win( board ).

    IF is_win = abap_true.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    " Checking rows, columns and diagonals for a win
    result = abap_false.
    LOOP AT board INTO DATA(line) FROM 1 TO 3.
      IF line CP 'XXX' OR line CP 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns
    DO 3 TIMES.
      IF board[1](sy-index) = board[2](sy-index) AND
         board[2](sy-index) = board[3](sy-index) AND
         board[1](sy-index) IS NOT INITIAL.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF ( board  = board  AND board  = board  OR
         board  = board  AND board  = board  ) AND
       board  IS NOT INITIAL.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    x_count = 0.
    o_count = 0.
    LOOP AT board INTO DATA(line).
      x_count = x_count + strlen( line ) - strlen( replace( line, 'X', '' ) ).
      o_count = o_count + strlen( line ) - strlen( replace( line, 'O', '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
