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
    METHODS: check_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(winner) TYPE player_type OPTIONAL,
    count_characters
      IMPORTING
        board   TYPE board_type
        char    TYPE c LENGTH 1
      RETURNING
        VALUE(count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_characters( board, 'X' ).
    o_count = count_characters( board, 'O' ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(winner) = check_winner( board ).

    IF winner IS NOT INITIAL.
      IF winner = 'X' AND x_count = o_count + 1 OR
         winner = 'O' AND x_count = o_count.
        state = state_enum-win.
      ELSE.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    " Checks for a winner in rows, columns, and diagonals
    LOOP AT board INTO DATA(row) FROM 1.
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ` `.
        winner = row(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    DO 3 TIMES.
      IF board[1]( sy-index ) = board[2]( sy-index ) AND board[2]( sy-index ) = board[3]( sy-index ) AND board[1]( sy-index ) <> ` `.
        winner = board[1]( sy-index ).
        RETURN.
      ENDDO.

    IF board  = board  AND board  = board  AND board  <> ` ` OR
       board  = board  AND board  = board  AND board  <> ` `.
      winner = board .
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    LOOP AT board INTO DATA(line).
      count += strlen( replace( val = line regex = '[^' && char && ']' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
