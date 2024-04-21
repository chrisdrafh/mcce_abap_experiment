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
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE string.
    METHODS count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
                VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.
    DATA winner TYPE string.

    count_marks( IMPORTING board = board
                 RETURNING x_count = x_count
                            o_count = o_count ).

    " Validate the turn order and count correctness
    IF o_count > x_count OR x_count - o_count > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_winner( board ).

    " Determine if someone has won and if the game should have stopped
    IF winner IS NOT INITIAL AND x_count + o_count <> 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for ongoing game or draw
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row) FROM 1 TO 3.
      " Check for horizontal win
      IF row+0(1) = row+1(1) AND row+1(1) = row+2(1) AND row+0(1) <> ' '.
        result = row+0(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check for vertical and diagonal wins
    DO 3 TIMES.
      " Vertical win
      IF board[1](sy-index) = board[2](sy-index) AND board[2](sy-index) = board[3](sy-index) AND board[1](sy-index) <> ' '.
        result = board[1](sy-index).
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonal wins
    IF board  = board  AND board  = board  AND board  <> ' '.
      result = board .
      RETURN.
    ELSIF board  = board  AND board  = board  AND board  <> ' '.
      result = board .
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_count( val = row regex = 'X' ) ).
      o_count += strlen( regex_count( val = row regex = 'O' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
