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
    DATA(x_count) = 0.
    DATA(o_count) = 0.

    " Count the number of X's and O's on the board
    (x_count, o_count) = count_marks( board ).

    " Check for a valid board state
    IF x_count - o_count NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid board: Wrong number of turns'.
    ENDIF.

    " Check if there is already a winner
    DATA(winner) = check_winner( board ).
    IF winner IS NOT INITIAL AND winner <> state_enum-ongoing_game.
      IF x_count <> o_count AND winner = player_enum-one OR
         x_count = o_count AND winner = player_enum-two.
        RAISE EXCEPTION TYPE cx_parameter_invalid
          EXPORTING text = 'Invalid board: Game continued after a win'.
      ENDIF.
      state = winner.
    ELSE.
      IF x_count + o_count = 9.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row).
      " Check rows
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ` `.
        RETURN player_enum-one IF row(1) = `X` ELSE player_enum-two.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      DATA(column_index) = sy-index.
      " Check columns
      IF board[1](column_index) = board[2](column_index) AND board[2](column_index) = board[3](column_index) AND board[1](column_index) <> ` `.
        RETURN player_enum-one IF board[1](column_index) = `X` ELSE player_enum-two.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF board  = board  AND board  = board  AND board  <> ` `.
      RETURN player_enum-one IF board  = `X` ELSE player_enum-two.
    ELSIF board  = board  AND board  = board  AND board  <> ` `.
      RETURN player_enum-one IF board  = `X` ELSE player_enum-two.
    ENDIF.

    result = state_enum-ongoing_game.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
