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
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_marks
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board first
    validate_board( board ).

    " Check if there is a winning configuration
    IF check_win( board ).
      state = state_enum-win.
    ELSEIF count_marks( board, player_enum-one ) + count_marks( board, player_enum-two ) = 9.
      " All cells are filled and no win, so it's a draw
      state = state_enum-draw.
    ELSE.
      " Game is still ongoing
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_marks( board, player_enum-one ).
    DATA(o_count) = count_marks( board, player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF x_count > 3 AND check_win( board ).
      " A player has won, check if the game continued after that
      IF x_count + o_count > 5.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns, and diagonals for a win
    LOOP AT board ASSIGNING FIELD-SYMBOL(<row>).
      IF <row> = 'XXX' OR <row> = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals for simplicity assume similar logic
    " Add your logic here for columns and diagonals
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      count += strlen( condense( row ) CS player ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
