CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3 WITH DEFAULT KEY.

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
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_marks
      IMPORTING board    TYPE board_type
      RETURNING VALUE(x_count) TYPE i
                VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board = board ).
    o_count = count_marks( board = board ).

    " Validate board
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid turn order or extra plays'.
    ENDIF.

    " Check for a winner
    DATA(winner) = check_winner( board ).
    IF winner IS NOT INITIAL.
      IF winner = player_enum-one AND x_count = o_count + 1 OR
         winner = player_enum-two AND x_count = o_count.
        state = state_enum-win.
      ELSE.
        RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Game continued after a win'.
      ENDIF.
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
      " Horizontal wins
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ` `.
        winner = row(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Vertical and diagonal wins
    DO 3 TIMES.
      " Vertical wins
      IF board[1]( sy-index ) = board[2]( sy-index ) AND
         board[2]( sy-index ) = board[3]( sy-index ) AND
         board[1]( sy-index ) <> ` `.
        winner = board[1]( sy-index ).
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonal wins
    IF ( board  = board  AND board  = board  OR
         board  = board  AND board  = board  ) AND
       board  <> ` `.
      winner = board .
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' case = 'i' ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' case = 'i' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
